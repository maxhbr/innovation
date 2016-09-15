{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.MetaGame
       ( UserId (..), isAdmin
       , UserC (..)
       , StateC (..), GameResult (..)
       , Log (..)
       , TransitionType, Transition (..)
       , ActionC (..), Action, does'
       , Game (..), play, extractGameResult, extractLog, extractState)
       where

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Monoid
import           Control.Monad.Identity
import           Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as W
import           Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as E
import           Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as S

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

-- ** Users and user-related stuff

data UserId = U String
            | Admin
            deriving (Show,Eq,Read)

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

class UserC user where
  getUserId :: user -> UserId
  isUserId :: UserId -> user -> Bool
  isUserId userId user = getUserId user == userId

-- ** State

class StateC state where
  initialState :: state
  getCurrentPlayer :: state -> UserId
  advancePlayerOrder :: state -> state

data GameResult = NoWinner
                | WinningOrder [UserId]
                deriving (Show,Eq,Read)

-- ** Transitions

newtype Log = Log (UserId -> Text)

type TransitionType s = StateT s (ExceptT Text (WriterT [Log] Identity)) GameResult

data Transition s = T { getTransition :: TransitionType s }

instance StateC state =>
         Monoid (Transition state) where
  mempty                = T $ return NoWinner
  mappend (T t1) (T t2) = T $ t1 >> t2

-- ** Actions

class (StateC state, Read actionToken, Show actionToken) =>
      ActionC state actionToken where
  toTransition' :: UserId -> actionToken -> Transition state
  isMetaAction' :: actionToken -> Bool
  isMetaAction' = const False

data Action state = forall actionToken.
                    ActionC state actionToken =>
                    Action { getActingPlayer :: UserId
                           , getToken :: actionToken }

instance Show (Action state) where
  show (Action Admin action)      = show action
  show (Action (U userId) action) = userId ++ ": " ++ show action

instance Eq (Action state) where
  act1 == act2 = show act1 == show act2

actionToTransition :: Action state -> Transition state
actionToTransition (Action userId actionToken) = toTransition' userId actionToken

does' :: ActionC state actionToken =>
        Proxy state -> UserId -> actionToken -> Action state
does' _ = Action

-- ** Game

newtype Game state = G [Action state]

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult state = (Either Text (GameResult, state), [Log])

play :: StateC state =>
        Game state -> PlayResult state
play (G game) = let
  fullTransition = getTransition $
                   mconcat $
                   map actionToTransition game
  in (runIdentity .
      W.runWriterT .
      E.runExceptT .
      S.runStateT fullTransition) initialState

-- ** related helper

extractGameResult :: PlayResult state -> GameResult
extractGameResult (Right (result,_),_) = result
extractGameResult _                    = NoWinner

extractLog :: PlayResult state -> [Log]
extractLog (_,log) = log

extractState :: PlayResult state -> Maybe state
extractState (Right (_,state),_) = Just state
extractState _                   = Nothing

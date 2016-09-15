{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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
import           Control.Monad.Writer (WriterT (..), Writer (..))
import qualified Control.Monad.Writer as W
import           Control.Monad.Except (ExceptT (..), MonadError (..))
import qualified Control.Monad.Except as E
import           Control.Monad.State.Lazy (State (..))
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

-- data Transition s = T (ExceptT Text (WriterT [Log] (State s)) GameResult)
-- data Transition s = T (Writer [Log] (State (Either Text s) GameResult))
type TransitionType s = WriterT [Log] (State s) GameResult
data Transition s = T { getTransition :: TransitionType s }
instance StateC state =>
         Monoid (Transition state) where
  mempty                = T $ S.put initialState >> return NoWinner
  mappend (T t1) (T t2) = T $ t1 >> t2

-- ** Actions

class (StateC state, Read actionToken, Show actionToken) =>
      ActionC state actionToken where
  toTransition' :: userId -> actionToken -> Transition state
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

play :: StateC state =>
        Game state -> ((GameResult, [Log]), state)
play (G game) = let
  fullTransition = getTransition $
                   mconcat $
                   map actionToTransition game
  in (S.runState . W.runWriterT) fullTransition initialState

extractGameResult :: ((GameResult, [Log]), state) -> GameResult
extractGameResult ((result,_),_) = result
extractLog :: ((GameResult, [Log]), state) -> [Log]
extractLog ((_,log),_) = log
extractState :: ((GameResult, [Log]), state) -> state
extractState (_, state) = state

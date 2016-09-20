{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.MetaGame
       ( IDAble (..)
       , UserId (..), UserC (..), isAdmin
       , PlayerOrder, StateC (..), GameResult (..)
       , Log (..), viewLog
       , TransitionType, Transition (..)
       , ActionC (..), Action, does'
       , Game (..)
       , actionToTransition, actionsToTransition
       , play, extractGameResult, extractLog, extractState
       , getCurrentPlayer
       , log, logForMe, logError
       , onlyAdminIsAllowed
       ) where

import           Prelude hiding (log)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Maybe
import           Data.Monoid
import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

class (Eq id, Show id, Read id) =>
      IDAble id a where
  getId :: a -> id
  hasId :: a -> id -> Bool
  hasId a id = getId a == id

--------------------------------------------------------------------------------
-- ** Users and user-related stuff

data UserId = U String
            | Admin
            deriving (Show,Eq,Read)
type UserC user = IDAble UserId user

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

--------------------------------------------------------------------------------
-- ** State

data GameResult = NoWinner
                | WinningOrder [UserId]
                deriving (Show,Eq,Read)

type PlayerOrder = [UserId]

class StateC state where
  emptyState :: state
  getCurrentPlayer' :: state -> UserId
  advancePlayerOrder :: state -> state
  getGameResult :: state -> GameResult
  getStateResult :: state -> (GameResult, state)
  getStateResult s = (getGameResult s, s)

--------------------------------------------------------------------------------
-- ** Transitions

newtype Log = Log (UserId -> Text)

viewLog :: UserId -> Log -> Text
viewLog userId (Log log) = log userId

instance Monoid Log where
  mempty                    = Log $ const (T.pack "")
  mappend (Log l1) (Log l2) = Log $ \userId -> let
    lo1 = l1 userId
    lo2 = l2 userId
    sep = T.pack $ if (not . T.null) lo1 && (not . T.null) lo2
                   then "\n"
                   else ""
    in T.concat [ l1 userId, sep ,l2 userId ]

type TransitionType s r = StateT s (ExceptT Text (WriterT Log Identity)) r

data Transition s = T { unpackTransition :: TransitionType s GameResult}

instance StateC state =>
         Monoid (Transition state) where
  mempty                = T $ return NoWinner
  mappend (T t1) (T t2) = T $ t1 >> t2

--------------------------------------------------------------------------------
-- ** ActionTokens and Actions

class (StateC state, Read actionToken, Show actionToken) =>
      ActionC state actionToken where
  toTransition' :: UserId -> actionToken -> Transition state

data Action state = forall actionToken.
                    ActionC state actionToken =>
                    Action { getActingPlayer :: UserId
                           , getToken :: actionToken }

instance Show (Action state) where
  show (Action Admin action)      = show action
  show (Action (U userId) action) = userId ++ ": " ++ show action

-- | The `Eq` instance of `Action state` is deriven from the `Show` instance
instance Eq (Action state) where
  act1 == act2 = show act1 == show act2

actionToTransition :: Action state -> Transition state
actionToTransition (Action userId actionToken) = toTransition' userId actionToken

actionsToTransition :: StateC state =>
                       [Action state] -> Transition state
actionsToTransition = mconcat . map actionToTransition

does' :: ActionC state actionToken =>
         Proxy state -> UserId -> actionToken -> Action state
does' _ = Action

--------------------------------------------------------------------------------
-- ** Game

newtype Game state = G [Action state]

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult state = (Either Text (GameResult, state), Log)

play :: StateC state =>
        Game state -> PlayResult state
play (G game) = let
  fullTransition = unpackTransition $
                   actionsToTransition game
  in (runIdentity .
      W.runWriterT .
      E.runExceptT .
      S.runStateT fullTransition) emptyState

--------------------------------------------------------------------------------
-- ** related helper

extractGameResult :: PlayResult state -> GameResult
extractGameResult (Right (result,_),_) = result
extractGameResult _                    = NoWinner

extractLog :: PlayResult state -> Log
extractLog (_,log) = log

extractState :: PlayResult state -> Maybe state
extractState (Right (_,state),_) = Just state
extractState _                   = Nothing

--------------------------------------------------------------------------------
-- * Other helper
--------------------------------------------------------------------------------

getCurrentPlayer :: StateC s =>
                    TransitionType s UserId
getCurrentPlayer = S.gets getCurrentPlayer'

log :: StateC s =>
       String -> TransitionType s ()
log text = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell .
      Log .
      const .
      T.pack $
      show loggingUser ++ ": " ++ text

logForMe :: StateC s =>
            String -> String -> TransitionType s ()
logForMe textPrivate textPublic = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell $ Log $
      \user -> T.pack $
               ((show loggingUser ++ ": ") ++) $
               if user == loggingUser || user == Admin
               then textPrivate
               else textPublic

logError :: StateC s =>
            String -> TransitionType s GameResult
logError error = do
  lift . lift . W.tell $ Log $ const $ T.pack $ "Error: " ++ error
  lift . E.throwE $ T.pack error
  S.state (\s -> (NoWinner, s))

onlyAdminIsAllowed :: UserId -> Transition state ->  Transition state
onlyAdminIsAllowed Admin t = t
onlyAdminIsAllowed _     _ = T $ lift . E.throwE $ T.pack "Action was not authorized"

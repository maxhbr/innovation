{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.MetaGame.Helper
       where
import           Prelude hiding (log)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Maybe
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Reader (Reader, ReaderT)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State.Lazy (State, StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import Game.MetaGame.Types

liftFromInner :: InnerMoveType s a -> MoveType s a
liftFromInner = lift . lift . lift

-- ** helper for logging

logAnEntry :: LogEntry -> MoveType s ()
logAnEntry = liftFromInner . logAnEntryI

log :: String -> MoveType s ()
log = liftFromInner . logI

loggs :: UserId -> String -> MoveType s ()
loggs uid = liftFromInner . loggsI uid

logggs :: UserId -> String -> String -> MoveType s ()
logggs uid unrestricted = liftFromInner . logggsI uid unrestricted

alog :: String -> String -> MoveType s ()
alog unrestricted = liftFromInner . alogI unrestricted

logA :: BoardC s =>
        String -> ActionType s ()
logA msg = do
  uid <- R.ask
  lift (uid `loggs` msg)

loggA :: BoardC s =>
         String -> String -> ActionType s ()
loggA unMsg restrMsg = do
  uid <- R.ask
  lift ((uid `logggs` unMsg) restrMsg)

-- | logWarn prints a warning to the log
logWarnI :: String -> InnerMoveType s ()
logWarnI warn = logI $ "Warning: " ++ warn

logWarn :: String -> MoveType s ()
logWarn = liftFromInner . logWarnI

-- | logError logs an error and throws the exception
-- this ends the game
logErrorI :: BoardC s =>
                 String -> InnerMoveType s a
logErrorI error = do
  logI $ "Error: " ++ error
  E.throwE $ T.pack error

logError :: BoardC s =>
            String -> MoveType s a
logError = liftFromInner . logErrorI

logErrorA :: BoardC s =>
             String -> ActionType s a
logErrorA err = do
  uid <- R.ask
  (lift . logError) (show uid ++ ": " ++ err)

-- | logFatal logs an fatal and throws the exception
-- this ends the game
logFatalI :: String -> InnerMoveType s a
logFatalI fatal = do
  logI $ "Fatal: " ++ fatal
  E.throwE $ T.pack fatal

logFatal :: BoardC s =>
            String -> MoveType s a
logFatal = liftFromInner . logFatalI

-- | logInfo
logInfoI :: BoardC s =>
          String -> InnerMoveType s ()
logInfoI info = logI $ "Info: " ++ info

logInfo :: BoardC s =>
           String -> MoveType s ()
logInfo = liftFromInner . logInfoI

-- | logTODO logs an unimplemented thing and throws the exception
-- this ends the game
logTODOI :: String -> InnerMoveType s a
logTODOI todo = do
  logI $ "TODO: " ++ todo
  E.throwE $ T.pack $ "TODO: " ++ todo

logTODO :: String -> MoveType s a
logTODO = liftFromInner . logTODOI

--------------------------------------------------------------------------------
-- * helper related to 'Move' and 'MoveType'-things
--------------------------------------------------------------------------------

-- ** helper for defining Moves and MoveType things

getMachineState :: BoardC b =>
                   MoveType b MachineState
getMachineState = S.gets getMachineState'

logMachineState :: BoardC s =>
                   MoveType s ()
logMachineState = getMachineState >>= (log . show)

--------------------------------------------------------------------------------
-- * helper related to 'Action'
--------------------------------------------------------------------------------

-- ** helper for defining Actions

-- liftToAType :: MoveWR board r -> ActionType board r
-- liftToAType = lift . unpackMove

-- -- | make an action
-- mToA :: MoveWR board r -> ActionWR board r
-- mToA = A . liftToAType

-- | make an action which is only allowed to be taken by admin
-- it is by definition user independent
mkAdminA :: BoardC board =>
            MoveWR board r -> ActionWR board r
mkAdminA t = A $ do
  uid <- R.ask
  case uid of
    Admin -> (lift . unpackMove) t
    _     -> logErrorA "Action was not authorized"

-- | make an action from an user-dependent MoveType-thing
mkA :: BoardC board =>
       (UserId -> MoveType board r) -> ActionWR board r
mkA f = A $ do
  uid <- R.ask
  lift (f uid)

-- | create an constant action which does not depend internally on the acting user
mkConstA :: BoardC board =>
            MoveType board r -> ActionWR board r
mkConstA = A . lift

-- | create an action which is only allowed to be taken by the currently action user
-- this might be unnecessary since 'play' does implicitly satisfy this
mkCurPlayerA :: BoardC board =>
                MoveType board r -> ActionWR board r
mkCurPlayerA t = A $ do
  uid <- R.ask
  currentPlayer <- (lift . S.gets) getCurrentPlayer'
  if currentPlayer == uid || uid == Admin
    then logErrorA "not allowed to take an action"
    else lift t

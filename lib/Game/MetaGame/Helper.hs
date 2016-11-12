{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.MetaGame.Helper
       ( logAnEntry, loggsAnEntry
       , log, loggs, logggs
       , alog, logA, loggA
       , logWarnI, logWarn
       , logErrorI, logError, logErrorA
       , logFatalI, logFatal
       , logInfoI, logInfo
       , logTODOI, logTODO
       , getMachineState, logMachineState
       , Actionable (..)
       ) where
import           Prelude hiding (log)
import qualified Data.Text as T
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S

import Game.MetaGame.Types

-- ** helper for logging

logAnEntry :: LogEntry -> MoveType s ()
logAnEntry = liftFromInner . logAnEntryI

loggsAnEntry :: UserId -> LogEntry -> MoveType s ()
loggsAnEntry uid = liftFromInner . (uid `loggsAnEntryI`)

log :: String -> MoveType s ()
log = liftFromInner . logI

loggs :: UserId -> String -> MoveType s ()
loggs uid = liftFromInner . loggsI uid

logggs :: UserId -> String -> String -> MoveType s ()
logggs uid unrestricted = liftFromInner . logggsI uid unrestricted

alog :: String -> String -> MoveType s ()
alog unrestricted = liftFromInner . alogI unrestricted

logA :: String -> ActionType s ()
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
logErrorI err = do
  logI $ "Error: " ++ err
  E.throwE $ T.pack err

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
logInfoI :: String -> InnerMoveType s ()
logInfoI info = logI $ "Info: " ++ info

logInfo :: String -> MoveType s ()
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

class BoardC board =>
      Actionable board a where
  toA :: forall r.
         a r -> ActionWR board r

  -- | make an action which is only allowed to be taken by admin
  -- it is by definition user independent
  mkAdminA :: a r -> ActionWR board r
  mkAdminA m = do
    uid <- A R.ask
    case uid of
      Admin -> toA m
      _     -> A $ logErrorA "Action was not authorized"

  -- | make an action from an user-dependent MoveType-thing
  mkA :: (UserId -> a r) -> ActionWR board r
  mkA f = do
    uid <- A R.ask
    toA (f uid)

  -- | create an action which is only allowed to be taken by the currently action user
  -- this might be unnecessary since 'play' does implicitly satisfy this
  mkCurPlayerA :: a r -> ActionWR board r
  mkCurPlayerA m = do
    uid <- A R.ask
    currentPlayer <- A $ (lift . S.gets) getCurrentPlayer'
    if currentPlayer == uid || uid == Admin
      then A $ logErrorA "not allowed to take an action"
      else toA m
instance BoardC board =>
         Actionable board (InnerMoveType board) where
  toA = toA . liftFromInner
instance BoardC board =>
         Actionable board (MoveType board) where
  toA = A . lift
instance BoardC board =>
         Actionable board (MoveWR board) where
  toA (M mv) = toA mv
instance BoardC board =>
         Actionable board (ActionType board) where
  toA = A
instance BoardC board =>
         Actionable board (ActionWR board) where
  toA = id

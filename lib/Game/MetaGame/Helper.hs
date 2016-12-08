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
       , logMachineState
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

logAnEntry :: LogEntry -> MoveType ()
logAnEntry = liftFromInner . logAnEntryI

loggsAnEntry :: UserId -> LogEntry -> MoveType ()
loggsAnEntry uid = liftFromInner . (uid `loggsAnEntryI`)

log :: String -> MoveType ()
log = liftFromInner . logI

loggs :: UserId -> String -> MoveType ()
loggs uid = liftFromInner . loggsI uid

logggs :: UserId -> String -> String -> MoveType ()
logggs uid unrestricted = liftFromInner . logggsI uid unrestricted

alog :: String -> String -> MoveType ()
alog unrestricted = liftFromInner . alogI unrestricted

logA :: String -> ActionType ()
logA msg = do
  uid <- R.ask
  lift (uid `loggs` msg)

loggA :: String -> String -> ActionType ()
loggA unMsg restrMsg = do
  uid <- R.ask
  lift ((uid `logggs` unMsg) restrMsg)

-- | logWarn prints a warning to the log
logWarnI :: String -> InnerMoveType ()
logWarnI warn = logI $ "Warning: " ++ warn

logWarn :: String -> MoveType ()
logWarn = liftFromInner . logWarnI

-- | logError logs an error and throws the exception
-- this ends the game
logErrorI :: String -> InnerMoveType a
logErrorI err = do
  logI $ "Error: " ++ err
  E.throwE $ T.pack err

logError :: String -> MoveType a
logError = liftFromInner . logErrorI

logErrorA :: String -> ActionType a
logErrorA err = do
  uid <- R.ask
  (lift . logError) (show uid ++ ": " ++ err)

-- | logFatal logs an fatal and throws the exception
-- this ends the game
logFatalI :: String -> InnerMoveType a
logFatalI fatal = do
  logI $ "Fatal: " ++ fatal
  E.throwE $ T.pack fatal

logFatal :: String -> MoveType a
logFatal = liftFromInner . logFatalI

-- | logInfo
logInfoI :: String -> InnerMoveType ()
logInfoI info = logI $ "Info: " ++ info

logInfo :: String -> MoveType ()
logInfo = liftFromInner . logInfoI

-- | logTODO logs an unimplemented thing and throws the exception
-- this ends the game
logTODOI :: String -> InnerMoveType a
logTODOI todo = do
  logI $ "TODO: " ++ todo
  E.throwE $ T.pack $ "TODO: " ++ todo

logTODO :: String -> MoveType a
logTODO = liftFromInner . logTODOI

--------------------------------------------------------------------------------
-- * helper related to 'Move' and 'MoveType'-things
--------------------------------------------------------------------------------

-- ** helper for defining Moves and MoveType things

logMachineState :: MoveType ()
logMachineState = getMachineState >>= (log . show)

--------------------------------------------------------------------------------
-- * helper related to 'Action'
--------------------------------------------------------------------------------

class Actionable a where
  toA :: forall r.
         a r -> ActionWR r

  -- | make an action which is only allowed to be taken by admin
  -- it is by definition user independent
  mkAdminA :: a r -> ActionWR r
  mkAdminA m = do
    uid <- A R.ask
    case uid of
      Admin -> toA m
      _     -> A $ logErrorA "Action was not authorized"

  -- | make an action from an user-dependent MoveType-thing
  mkA :: (UserId -> a r) -> ActionWR r
  mkA f = do
    uid <- A R.ask
    toA (f uid)

  -- | create an action which is only allowed to be taken by the currently action user
  -- this might be unnecessary since 'play' does implicitly satisfy this
  mkCurPlayerA :: a r -> ActionWR r
  mkCurPlayerA m = do
    uid <- A R.ask
    currentPlayer <- A $ lift getCurrentPlayer
    if currentPlayer == uid || uid == Admin
      then A $ logErrorA "not allowed to take an action"
      else toA m
instance Actionable InnerMoveType where
  toA = toA . liftFromInner
instance Actionable MoveType where
  toA = A . lift
instance Actionable MoveWR where
  toA (M mv) = toA mv
instance Actionable ActionType where
  toA = A
instance Actionable ActionWR where
  toA = id

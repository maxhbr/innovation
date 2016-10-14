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

--------------------------------------------------------------------------------
-- * helper related to 'Move' and 'MoveType'-things
--------------------------------------------------------------------------------

liftFromInner :: InnerMoveType s a -> MoveType s a
liftFromInner = lift . lift . lift

clog' :: (Monad m, MonadTrans t) =>
        String -> t (WriterT Log m) ()
clog' = lift . W.tell . clog

-- ** helper for logging
log :: BoardC s =>
       String -> MoveType s ()
log text = liftFromInner . clog' $ "... " ++ text

logM :: BoardC s =>
        String -> ActionType s ()
logM msg = do
  uid <- R.ask
  lift (log (show uid ++ ": " ++ msg))

-- logBy :: BoardC s =>
--          UserId -> String -> MoveType s ()
-- logBy loggingUser text = liftFromInner . clog' $
--                        show loggingUser ++ ": " ++ text
loggs :: UserId -> String -> MoveType s ()
loggs loggingUser text = liftFromInner . clog' $ show loggingUser ++ ": " ++ text

logForMe :: BoardC s =>
            UserId -> String -> String -> MoveType s ()
logForMe loggingUser textPrivate textPublic = liftFromInner . lift . W.tell . Log $ do
  viewer <- R.ask
  return (T.pack (show loggingUser ++ ": " ++
                  if viewer == loggingUser || viewer == Admin
                  then textPrivate
                  else textPublic))

-- | logWarn prints a warning to the log
logWarn :: BoardC s =>
            String -> MoveType s ()
logWarn = liftFromInner . innerLogWarn

innerLogWarn :: BoardC s =>
                 String -> InnerMoveType s ()
innerLogWarn warn = clog' $ "Warning: " ++ warn

innerLogError :: BoardC s =>
                 String -> InnerMoveType s a
innerLogError error = do
  clog' $ "Error: " ++ error
  E.throwE $ T.pack error

-- | logError logs an error and throws the exception
-- this ends the game
logError :: BoardC s =>
            String -> MoveType s a
logError = liftFromInner . innerLogError

logErrorM :: BoardC s =>
             String -> ActionType s a
logErrorM err = do
  uid <- R.ask
  (lift . logError) (show uid ++ ": " ++ err)

-- | logFatal logs an fatal and throws the exception
-- this ends the game
logFatal :: BoardC s =>
            String -> MoveType s a
logFatal = liftFromInner . innerLogFatal

innerLogFatal :: BoardC s =>
                 String -> InnerMoveType s a
innerLogFatal fatal = do
  clog' $ "Fatal: " ++ fatal
  E.throwE $ T.pack fatal

-- | logInfo
logInfo :: BoardC s =>
           String -> MoveType s ()
logInfo = liftFromInner . innerLogInfo

innerLogInfo :: BoardC s =>
                String -> InnerMoveType s ()
innerLogInfo info = clog' $ "Info: " ++ info

-- | logTODO logs an unimplemented thing and throws the exception
-- this ends the game
logTODO :: BoardC s =>
           String -> MoveType s a
logTODO = liftFromInner . innerLogTODO

innerLogTODO :: BoardC s =>
                String -> InnerMoveType s a
innerLogTODO todo = do
  clog' $ "TODO: " ++ todo
  E.throwE $ T.pack $ "TODO: " ++ todo

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
    _     -> logErrorM "Action was not authorized"

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
  if (currentPlayer == uid || uid == Admin)
    then logErrorM "not allowed to take an action"
    else lift t

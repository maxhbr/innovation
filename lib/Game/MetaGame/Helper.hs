{-# LANGUAGE LambdaCase #-}
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
import           Control.Monad.Trans.State.Lazy (State, StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import Game.MetaGame.Types

--------------------------------------------------------------------------------
-- * helper related to 'Move' and 'MoveType'-things
--------------------------------------------------------------------------------

log' :: (Monad m, MonadTrans t) =>
        String -> t (WriterT Log m) ()
log' = lift . W.tell . clog

-- ** helper for logging
log :: BoardC s =>
       String -> MoveType s ()
log text = do
    loggingUser <- getCurrentPlayer
    lift . log' $
      pp loggingUser ++ ": " ++ text

logForMe :: BoardC s =>
            String -> String -> MoveType s ()
logForMe textPrivate textPublic = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell . Log $
      \user -> T.pack $
               ((pp loggingUser ++ ": ") ++) $
               if user == loggingUser || user == Admin
               then textPrivate
               else textPublic

-- | logWarn prints a warning to the log
logWarn :: BoardC s =>
            String -> MoveType s ()
logWarn = lift . innerLogWarn

innerLogWarn :: BoardC s =>
                 String -> InnerMoveType s ()
innerLogWarn warn = log' $ "Warning: " ++ warn


-- | logError logs an error and throws the exception
-- this ends the game
logError :: BoardC s =>
            String -> MoveType s a
logError = lift . innerLogError

innerLogError :: BoardC s =>
                 String -> InnerMoveType s a
innerLogError error = do
  log' $ "Error: " ++ error
  E.throwE $ T.pack error

-- | logTODO logs an unimplemented thing and throws the exception
-- this ends the game
logTODO :: BoardC s =>
           String -> MoveType s a
logTODO = lift . innerLogTODO

innerLogTODO :: BoardC s =>
           String -> InnerMoveType s a
innerLogTODO todo = do
  log' $ "TODO: " ++ todo
  E.throwE $ T.pack $ "TODO: " ++ todo

-- ** helper for defining Moves and MoveType things

getCurrentPlayer :: BoardC s =>
                    MoveType s UserId
getCurrentPlayer = S.gets getCurrentPlayer'

--------------------------------------------------------------------------------
-- * helper related to 'Action'
--------------------------------------------------------------------------------

-- ** helper for defining Actions

onlyAdminIsAllowed :: BoardC board =>
                      Action board -> Action board
onlyAdminIsAllowed (A t) = A $ \case
  Admin -> t Admin
  _     -> M $ logError "Action was not authorized"

onlyCurrentPlayerIsAllowed :: BoardC board =>
                              Action board -> Action board
onlyCurrentPlayerIsAllowed (A t) = A $ \userId -> M $ do
  currentPlayer <- S.gets getCurrentPlayer'
  if (currentPlayer == userId || userId == Admin)
    then logError $ "Player " ++ show userId ++ " is not allowed to take an action"
    else unpackMove $ t userId

mkA :: (UserId -> MoveType board r) -> ActionWR board r
mkA f = A $ \userId -> M $ f userId

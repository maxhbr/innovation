module Game.MetaGame.Play
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

import           Game.MetaGame.Types
import           Game.MetaGame.Helper

unpackToken :: ActionToken board actionToken =>
               actionToken -> UserId -> MoveWR board ()
unpackToken token userId = do
  b <- isAllowedFor token userId
  M $ if b
      then userId `takes` (getAction token)
      else logError ("user " ++ show userId ++ " is not allowed to " ++ show token)

generateNextTurnMessage :: BoardC board =>
                           board -> InnerMoveType board board
generateNextTurnMessage = undefined -- TODO

-- | run a turn on a board
-- this also advances the player order, i.e. consumes an 'action'
runTurn :: BoardC board =>
           board -> Turn board ->InnerMoveType board board
runTurn b0 turn@(Turn userId actionToken choices) = do
  case (getMachineState' b0) of
    Prepare -> unless (userId == Admin) $
                 innerLogError "only admin is allowed to take turns in the prepare phase"
    WaitForTurn -> unless (getCurrentPlayer' b0 == userId) $
                     innerLogError $ "the player " ++ show userId ++ " is not allowed to take an action"
    WaitForChoice inq -> innerLogError $ "still waiting for answers to: " ++ (show inq)
    GameOver _ -> innerLogError "game already over"

  let move = unpackMove (unpackToken actionToken userId)

  result <- runOuterMoveType b0 choices move
  case result of
    Left b1 -> do -- ^ Turn not yet completed
      return b1
    Right ((_, b1), unconsumedChoices) -> do -- ^ Turn completed
      unless (null unconsumedChoices) $
        innerLogError "not all choices were cosumed"

      (lift . lift . W.tell) (G [turn])
      return $ advancePlayerOrder b1

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult board = InnerMoveResult board board

-- | one is able to play a game
play :: BoardC board =>
        Game board -> PlayResult board
play (G history)= (runInnerMoveType . foldM runTurn emptyBoard . reverse) history

--------------------------------------------------------------------------------
-- ** related helper

extractLog :: PlayResult board -> Log
extractLog ((_,log),_) = log

extractGame :: PlayResult board -> Game board
extractGame (_, g) = g

extractBoard :: PlayResult board -> Maybe board
extractBoard ((Right board,_),_) = Just board
extractBoard _                   = Nothing

extractWinner :: BoardC board =>
                     PlayResult board -> Maybe UserId
extractWinner r = case extractBoard r of
  Just b -> getWinner b
  _      -> Nothing

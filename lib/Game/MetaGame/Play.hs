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
import           Control.Monad.Trans.State.Lazy (State, StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import           Game.MetaGame.Types

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult board = InnerMoveResult board board

-- | one is able to play a game
play :: BoardC board =>
        Game board -> PlayResult board
play (G turns)= (runInnerMoveType . play') (reverse turns)
  where
    play' :: BoardC board =>
             [Turn board] -> InnerMoveType board board
    play' = foldM applyTurn emptyBoard

    logError' :: String -> InnerMoveType s a
    logError' error = do
      lift . W.tell $ clog error
      E.throwE $ T.pack error

    applyTurn :: BoardC board =>
                 board -> Turn board -> InnerMoveType board board
    applyTurn b0 turn = do
      when (hasWinner b0) $
        logError' $ "game already over"
      let currentPlayer = getCurrentPlayer' b0
      let actingPlayer  = getActingPlayer turn
      (lift . lift . S.modify) (\ (G g) -> G $ turn : g)
      case currentPlayer == actingPlayer of
        True -> runTurn turn b0
        False -> logError' $ "the player " ++ pp actingPlayer ++ " is not allowed to take an action"

--------------------------------------------------------------------------------
-- ** related helper

extractLog :: PlayResult board -> Log
extractLog ((_,log),_) = log

extractBoard :: PlayResult board -> Maybe board
extractBoard ((Right board,_),_) = Just board
extractBoard _                   = Nothing

extractGameResult :: BoardC board =>
                     PlayResult board -> GameResult
extractGameResult r = case extractBoard r of
  Just b -> getGameResult b
  _      -> NoWinner

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
import           Game.MetaGame.Helper


-- | run a turn on a board
-- this also advances the player order, i.e. consumes an 'action'
runTurn :: BoardC board =>
           Turn board -> board -> InnerMoveType board board
runTurn (Turn userId actionToken choices) b0 = do
  ((_, b1), restChoices) <- runOuterMoveType b0 choices ( unpackMove
                                                          ( unpackToken actionToken userId ) )
  unless (restChoices == []) $
    innerLogError "not all choices were cosumed"
  -- TODO: produce message, who is next and what is to do
  return $ advancePlayerOrder b1

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult board = InnerMoveResult board board

-- | one is able to play a game
play :: BoardC board =>
        Game board -> PlayResult board
play (G history)= (runInnerMoveType . play' . reverse) history
  where

    play' :: BoardC board =>
             [Turn board] -> InnerMoveType board board
    play' = foldM applyTurn emptyBoard

    applyTurn :: BoardC board =>
                 board -> Turn board -> InnerMoveType board board
    applyTurn b0 turn = do
      when (hasWinner b0) $
        innerLogError $ "game already over"

      let currentPlayer = getCurrentPlayer' b0
      let actingPlayer  = getActingPlayer turn

      case currentPlayer == actingPlayer of
        True -> do
          r <- runTurn turn b0
          --  this drops all choices incl. action if only one is invalid?
          (lift . lift . W.tell) (G [turn])
          return r
        False -> innerLogError $ "the player " ++ pp actingPlayer ++ " is not allowed to take an action"

--------------------------------------------------------------------------------
-- ** related helper

extractLog :: PlayResult board -> Log
extractLog ((_,log),_) = log

extractGame :: PlayResult board -> Game board
extractGame (_, g) = g

extractBoard :: PlayResult board -> Maybe board
extractBoard ((Right board,_),_) = Just board
extractBoard _                   = Nothing

extractGameResult :: BoardC board =>
                     PlayResult board -> GameResult
extractGameResult r = case extractBoard r of
  Just b -> getGameResult b
  _      -> NoWinner

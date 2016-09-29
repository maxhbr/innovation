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
play (G history)= (runInnerMoveType . play') (accumulateChoices history)
  where
    accumulateChoices :: History board -> [(Turn board, [Choice])]
    accumulateChoices = reverse . accumulateChoices' []
      where
        accumulateChoices' :: [Choice] -> History board -> [(Turn board, [Choice])]
        accumulateChoices' cs ((HChoice c):hs) = accumulateChoices' (c:cs) hs
        accumulateChoices' cs ((HTurn t):hs)   = (t, cs) : accumulateChoices' [] hs
        accumulateChoices' _  []               = [] -- drop trailing choices

    play' :: BoardC board =>
             [(Turn board, [Choice])] -> InnerMoveType board board
    play' = foldM applyTurn emptyBoard

    applyTurn :: BoardC board =>
                 board -> (Turn board, [Choice]) -> InnerMoveType board board
    applyTurn b0 (turn, choices) = do
      when (hasWinner b0) $
        logError' $ "game already over"

      let currentPlayer = getCurrentPlayer' b0
      let actingPlayer  = getActingPlayer turn

      case currentPlayer == actingPlayer of
        True -> do
          r <- runTurn turn b0
          --  this drops all choices incl. action if only one is invalid?
          (lift . lift . S.modify) (\ (G g) -> G $ HTurn turn : g)
          mapM_ (\c -> (lift . lift . S.modify) (\ (G g) -> G $ HChoice c : g)) choices
          return r
        False -> logError' $ "the player " ++ pp actingPlayer ++ " is not allowed to take an action"

    logError' :: String -> InnerMoveType s a
    logError' error = do
      lift . W.tell $ clog error
      E.throwE $ T.pack error

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

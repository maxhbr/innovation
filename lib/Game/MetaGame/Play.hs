module Game.MetaGame.Play
       ( PlayResult
         -- play
       , play
         -- helper functions
       , extractLog, extractGame, extractBoard, extractWinner
       ) where
import           Prelude hiding (log)
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Writer as W
import qualified System.HsTColors as HsT

import           Game.MetaGame.Types
import           Game.MetaGame.Helper

unpackToken :: ActionToken board actionToken =>
               actionToken -> UserId -> MoveWR board ()
unpackToken token userId = M $ do
  b <- stateMatchesExpectation token
  if b
    then userId `takes` (getAction token)
    else logError ("user " ++ show userId ++ " is not allowed to " ++ show token)

generateNextTurnMessage :: BoardC board =>
                           board -> InnerMoveType board board
generateNextTurnMessage board = do
  logAnEntryI (("\n" ++ HsT.mkRed "<= ")
               <<> (case (getMachineState' board) of
                       Prepare -> view Admin <>> " should continue to setup the game"
                       WaitForTurn -> view (getCurrentPlayer' board) <>> " should take an action"
                       WaitForChoice inq -> ((view (askedPlayer inq)) <>> "some user should answer: ") <> (view inq)
                       GameOver winner -> "game already over, " <<> (view winner) <>> " has won"
                   ))
  return board

-- | run a turn on a board
-- this also advances the player order, i.e. consumes an 'action'
runTurn :: BoardC board =>
           board -> Turn board -> InnerMoveType board board
runTurn b0 turn@(Turn userId actionToken choices) = do
  logAnEntryI ((HsT.mkGreen "=> ") <<> view turn)
  case (getMachineState' b0) of
    Prepare ->
      unless (userId == Admin) $
        logErrorI "only admin is allowed to take turns in the prepare phase"
    WaitForTurn ->
      unless (getCurrentPlayer' b0 == userId) $
        logErrorI $ "the player " ++ show userId ++ " is not allowed to take an action"
    WaitForChoice inq ->
      logErrorI $ "still waiting for answers to: " ++ show inq
    GameOver _ ->
      logErrorI "game already over"

  let move = unpackMove (unpackToken actionToken userId)

  result <- runOuterMoveType b0 choices move
  case result of
    Left b1 -> -- ^ Turn not yet completed
      return b1
    Right ((_, b1), unconsumedChoices) -> do -- ^ Turn completed
      unless (null unconsumedChoices) $
        logErrorI "not all choices were cosumed"

      (lift . lift . W.tell) (G [turn])
      return (if (userId == Admin)
              then b1
              else advancePlayerOrder b1)

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult board = InnerMoveResult board board

-- | one is able to play a game
play :: BoardC board =>
        Game board -> PlayResult board
play (G history) = runInnerMoveType $
  (foldM runTurn emptyBoard . reverse) history >>= generateNextTurnMessage

--------------------------------------------------------------------------------
-- ** related helper

extractLog :: PlayResult board -> Log
extractLog ((_,l),_) = l

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

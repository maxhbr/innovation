module Game.MetaGame.Play
       ( PlayResult
         -- play
       , play
         -- helper functions
       , extractLog, extractGame, extractGameState, extractWinner, extractCurrentPlayer
       ) where
import           Prelude hiding (log)
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Writer as W
import qualified System.HsTColors as HsT

import           Game.MetaGame.Types
import           Game.MetaGame.Helper
import           Game.MetaGame.GameRules
import qualified Game.MetaGame.Types.GameState as GS

unpackToken :: ActionToken actionToken =>
               actionToken -> UserId -> MoveWR ()
unpackToken token userId = M $ do
  b <- stateMatchesExpectation token
  if b
    then userId `takes` getAction token
    else logError ("user " ++ show userId ++ " is not allowed to " ++ show token)

-- generateNextTurnMessage :: BoardC board =>
--                            board -> InnerMoveType board
-- generateNextTurnMessage board = do
--   logAnEntryI (("\n" ++ HsT.mkRed "<= ")
--                <<> (case (getMachineState' board) of
--                        Prepare -> view Admin <>> " should continue to setup the game"
--                        WaitForTurn -> view (getCurrentPlayer' board) <>> " should take an action"
--                        WaitForChoice inq -> ((view (askedPlayer inq)) <>> "some user should answer: ") <> (view inq)
--                        GameOver winner -> "game already over, " <<> (view winner) <>> " has won"
--                    ))
--   return board

-- | run a turn on a board
-- this also advances the player order, i.e. consumes an 'action'
runTurn :: Rules -> GameState -> Turn -> InnerMoveType GameState
runTurn rules gs0@(GameState _ ms0 cp0) turn@(Turn userId actionToken choices) = do
  logAnEntryI ((HsT.mkGreen "=> ") <<> view turn)
  case ms0 of
    Prepare ->
      unless (userId == Admin) $
        logErrorI "only admin is allowed to take turns in the prepare phase"
    WaitForTurn ->
      unless (cp0 == userId) $
        logErrorI $ "the player " ++ show userId ++ " is not allowed to take an action"
    WaitForChoice inq ->
      logErrorI $ "still waiting for answers to: " ++ show inq
    GameOver _ ->
      logErrorI "game already over"

  let move = unpackMove (unpackToken actionToken userId)

  result <- runOuterMoveType gs0 choices move
  case result of
    Left gs1 -> -- ^ Turn not yet completed
      return gs1
    Right ((_, gs1), unconsumedChoices) -> do -- ^ Turn completed
      unless (null unconsumedChoices) $
        logErrorI "not all choices were cosumed"

      (lift . lift . W.tell) (G [turn])
      return (if userId == Admin
              then gs1
              else advancePlayerOrder rules gs1)

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

type PlayResult = InnerMoveResult GameState

-- | one is able to play a game
play :: Rules -> Game -> PlayResult
play rules (G history) = runInnerMoveType $
  (foldM (runTurn rules) emptyGameState . reverse) history

--------------------------------------------------------------------------------
-- ** related helper

extractLog :: PlayResult -> Log
extractLog ((_,l),_) = l

extractGame :: PlayResult -> Game
extractGame (_, g) = g

extractGameState :: PlayResult -> Maybe GameState
extractGameState ((gsOrT,_),_) = case gsOrT of
  Right gs -> Just gs
  _        -> Nothing

extractWinner :: PlayResult -> Maybe UserId
extractWinner pr = case unpackInnerMoveResult pr of
  Just GameState { _machineState = GameOver winner} -> Just winner
  _                                                 -> Nothing

extractCurrentPlayer :: PlayResult -> Maybe UserId
extractCurrentPlayer = (fmap GS.getCurrentPlayer) . extractGameState

{-# LANGUAGE ExistentialQuantification #-}
module Game.MetaGame.Types.GameState
       where

import Game.MetaGame.Types.Core

data MachineState
  = Prepare -- ^ the game has not yet been started and only 'Admin' should take actions
  | WaitForTurn -- ^ last turn was done completly and current player should choose his action
  -- | forall a.
  --   (Show a) =>
  --   WaitForChoice (Inquiry a) -- ^ current turn requires more imput, which needs to be provided by some user
  | GameOver UserId -- ^ there was already an game ending situation and mentioned user has won, nothing should be able to change the current board
  deriving (Eq, Show)

data GameState
  = GameState
    { _world         :: World
    , _machineState  :: MachineState
    , _currentPlayer :: UserId }
emptyGameState :: GameState
emptyGameState = GameState (World []) Prepare Admin


-- * IDs which allow to access special elements in the World
mainBoardId, playersId :: String
mainBoardId = "mainBoardId"
playersId = "playersId"

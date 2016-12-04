{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Game.MetaGame.Types.Board
       ( BoardC (..)
       , MachineState (..), MachineStateId (..)
       ) where

import           Data.Text (Text)
import           Control.Monad.Trans.Except (Except)

import           Game.MetaGame.Types.Core
import           Game.MetaGame.Types.Inquiry

data MachineState
  = Prepare -- ^ the game has not yet been started and only 'Admin' should take actions
  | WaitForTurn -- ^ last turn was done completly and current player should choose his action
  | forall a.
    (Show a) =>
    WaitForChoice (Inquiry a) -- ^ current turn requires more imput, which needs to be provided by some user
  | GameOver UserId -- ^ there was already an game ending situation and mentioned user has won, nothing should be able to change the current board

instance Eq MachineState where
  Prepare           == Prepare           = True
  WaitForTurn       == WaitForTurn       = True
  (WaitForChoice _) == (WaitForChoice _) = True
  (GameOver gr1)    == (GameOver gr2)    = gr1 == gr2
  _                 == _                 = False

instance Show MachineState where
  show Prepare             = "Prepare"
  show WaitForTurn         = "WaitForTurn"
  show (WaitForChoice inq) = "WaitForChoice: " ++ show inq
  show (GameOver winner)   = "GameOver: " ++ show winner ++ " has won"
instance View MachineState where
  getOwner (WaitForChoice inq) = getOwner inq
  getOwner _                   = Guest
  showRestricted (WaitForChoice inq) = "WaitForChoice: " ++ showRestricted inq
  showRestricted ms                  = show ms

data MachineStateId = MachineStateId
                    deriving (Eq,Show)
type instance IdF MachineState = MachineStateId
instance IdAble MachineState where
  idOf _ = MachineStateId

class IdAble board =>
      BoardC board where
  emptyBoard :: board

  -- | metainformation on the state of the board
  getMachineState' :: board -> MachineState

  -- | set the metainfo
  setMachineState' :: MachineState -> board -> board

  -- | get the player, which is expected to act next (take a turn, give an answer, ...)
  -- if another user is acting, the game fails
  getCurrentPlayer' :: board -> UserId

  -- | the current turn is done
  advancePlayerOrder :: board -> board

  -- | atomic update
  -- this should check for winning conditions and do all other checks, which
  -- depend on the current state and could happen in mid turn
  doAtomicUpdate :: board -> Except Text board

  -- | unpack the currently known result from the board
  -- If there is a winner, the board should know about it
  getWinner :: board -> Maybe UserId
  getWinner b = case getMachineState' b of
    GameOver winner -> Just winner
    _               -> Nothing

  -- | determine whether the game has already a winner and thus is ended
  hasWinner :: board -> Bool
  hasWinner board = case getWinner board of
    Just _ -> True
    Nothing -> False

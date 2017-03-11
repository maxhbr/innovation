module Game.MetaGame.GameRules
       ( Rules (..)
       , advancePlayerOrder
       ) where
import           Game.MetaGame.Types.GameState (setCurrentPlayer)
import           Game.MetaGame.Types hiding (setCurrentPlayer)

data Rules
  = Rules
    { getNextPlayer         :: GameState -> UserId
    , determineWinner       :: MoveType (Maybe UserId)
    -- , isGameStateConsistend :: GameState -> Bool
    , atomicUpdate          :: MoveType ()
    }

advancePlayerOrder :: Rules -> GameState -> GameState
advancePlayerOrder Rules{getNextPlayer=gnp} gs = setCurrentPlayer (gnp gs) gs

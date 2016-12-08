module Game.MetaGame.Types.GameRules
       ( Rules (..)
       , advancePlayerOrder
       ) where
import           Game.MetaGame.Types.Core hiding (getObject, setObject, modifyObject)
import           Game.MetaGame.Types.GameState

data Rules
  = Rules
    { initilState           :: GameState
    , getNextPlayer         :: GameState -> UserId
    , determineWinner       :: GameState -> UserId
    , isGameStateConsistend :: GameState -> Bool
    , doAtomicUpdate        :: GameState -> GameState
    }

advancePlayerOrder :: Rules -> GameState -> GameState
advancePlayerOrder Rules{getNextPlayer=gnp} gs = setCurrentPlayer (gnp gs) gs

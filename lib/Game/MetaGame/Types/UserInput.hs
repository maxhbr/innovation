module Game.MetaGame.Types.UserInput
       ( UserInput (..)
       , does, chooses
       , mkG, (<=>)
       ) where

import           Game.MetaGame.Types.Core
import           Game.MetaGame.Types.GameState
import           Game.MetaGame.Types.Game

data UserInput
  = UTurn Turn
  | UChoice (Turn -> Turn)

does :: ActionToken actionToken =>
        UserId -> actionToken -> UserInput
does uid t = UTurn $ Turn uid t []

chooses :: UserId -> (UserId -> Answer) -> UserInput
chooses uid c = UChoice $ \t -> t{ answers=c uid : answers t }

mkG :: [UserInput] -> Game
mkG = G . accumulateInput id . reverse
  where
    accumulateInput :: (Turn -> Turn) -> [UserInput] -> [Turn]
    accumulateInput cs (UChoice f : uis) = accumulateInput (f . cs) uis
    accumulateInput cs (UTurn t : uis)   = cs t : accumulateInput id uis
    accumulateInput _  []                = []

(<=>) :: Game -> UserInput -> Game
(G (t:ts)) <=> (UChoice f) = G $ f t : ts
(G [])     <=> (UChoice _) = G $ [] -- TODO: this is an error
(G ts)     <=> (UTurn t)   = G $ t : ts

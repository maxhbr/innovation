module Game.MetaGame.Types.UserInput
       ( UserInput (..)
       , does, chooses
       , mkG, (<=>)
       ) where

import           Data.Proxy

import           Game.MetaGame.Types.Core
import           Game.MetaGame.Types.Inquiry
import           Game.MetaGame.Types.Game

data UserInput board
  = UTurn (Turn board)
  | UChoice (Turn board -> Turn board)

does :: ActionToken board actionToken =>
        Proxy board -> UserId -> actionToken -> UserInput board
does _ uid t = UTurn $ Turn uid t []

chooses :: Proxy board -> UserId -> (UserId -> Answer) -> UserInput board
chooses _ uid c = UChoice $ \t -> t{ answers=c uid : answers t }

mkG :: [UserInput board] -> Game board
mkG = G . accumulateInput id . reverse
  where
    accumulateInput :: (Turn board -> Turn board) -> [UserInput board] -> [Turn board]
    accumulateInput cs (UChoice f : uis) = accumulateInput (f . cs) uis
    accumulateInput cs (UTurn t : uis)   = cs t : accumulateInput id uis
    accumulateInput _  []                = []

(<=>) :: Game board -> UserInput board -> Game board
(G (t:ts)) <=> (UChoice f) = G $ f t : ts
(G [])     <=> (UChoice _) = G $ [] -- TODO: this is an error
(G ts)     <=> (UTurn t)   = G $ t : ts

module Game.MetaGame.Game
       where

import Game.MetaGame.Action

type History
  = [Action]
data Game
  = G Rules History

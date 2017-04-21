module Game.MetaGame.Game
       where

import Game.MetaGame.Action

data Rules

type History
  = [Action ()]
data Game
  = G Rules History

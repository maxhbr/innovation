module Game.Innovation.BaseTypes
       where

data Color = Blue | Purple | Red | Yellow | Green
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
colors :: [Color]
colors = [minBound ..]

data Age = Age1 | Age2 | Age3 | Age4 | Age5 | Age6 | Age7 | Age8 | Age9 | Age10
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
ages :: [Age]
ages =  [minBound ..]

data Symbol = Castle | Tree | Crown | Bulb | Factory | Clock
  deriving (Eq,Show,Read,Enum,Ord,Bounded)

module Game.Burgen.Actions
  where

import           Game.MetaGame

type WorkerCount = Int

-- | get a tile from the board to the own waiting area
data GetTile
  = GetTile DieRoll WorkerCount Position
instance ActionToken GetTile where
  getAction = undefined

-- | place a tile the own waiting area to the own board
data PlaceTile
  = PlaceTile DieRoll WorkerCount Tile Position
instance ActionToken PlaceTile where
  getAction = undefined

-- | sell one kind of goods
data SellGoods
  = SellGoods Good
instance ActionToken SellGoods where
  getAction = undefined

-- | get two workers for one die
data Pass
  = Pass DieRoll
instance ActionToken Pass where
  getAction = undefined

-- | BuyTile
-- free action
data BuyTile
  = BuyTile Position
instance ActionToken BuyTile where
  getAction = undefined

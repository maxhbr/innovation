module Game.Burgen.Types where

data Color

data DieRoll
  = DieRoll1
  | DieRoll2
  | DieRoll3
  | DieRoll4
  | DieRoll5
  | DieRoll6
  deriving (Eq, Enum, Show, Read)

data GrassTileKind
  = Cow
  | Chicken
  | Sheep
  deriving (Eq, Enum, Show, Read)
data TownTileKind
  = TownTileKind
  deriving (Eq, Enum, Show, Read)
data ScienceTileKind
  = ScienceTileKind
  deriving (Eq, Enum, Show, Read)
data Tile
  = GrassTile GrassTileKind
              Int -- ^ count
  | TownTile TownTileKind
  | ScienceTile ScienceTileKind
  | ShipTile
  | CastleTile
  | MineTile
  | BlackMarket Tile
  | GenTile String -- ^ remove
  deriving (Eq, Show, Read)

data TileKind
  = GrassKind
  | TownKind
  | ScienceKind
  | ShipKind
  | CastleKind
  | MineKind
  | GenKind -- ^ remove
  deriving (Eq, Enum, Show, Read)

getKind :: Tile -> TileKind
getKind GrassTile{}   = GrassKind
getKind TownTile{}    = TownKind
getKind ScienceTile{} = ScienceKind
getKind ShipTile{}    = ShipKind
getKind CastleTile{}  = CastleKind
getKind MineTile{}    = MineKind
getKind GenTile{}     = GenKind

data Good
  = Good1
  | Good2
  | Good3
  | Good4
  | Good5
  | Good6

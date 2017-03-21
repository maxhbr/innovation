module Game.Burgen.Board
  where

import qualified Data.Map as Map
import           Data.Map (Map)

import Game.Burgen.Types
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal

-- see https://github.com/mhwombat/grid/wiki/Hexagonal-tiles
-- see http://hackage.haskell.org/package/grid-7.8.8/docs/Math-Geometry-Grid-Hexagonal.html
-- see http://hackage.haskell.org/package/grid-7.8.8/docs/Math-Geometry-GridInternal.html#t:Grid

class (Eq playerBoard, Read playerBoard, Show playerBoard) =>
      PlayerBoard playerBoard where
  makeGrid :: playerBoard -> HexHexGrid
  makeGrid _ = hexHexGrid 4

  getKindAt :: playerBoard -> Index HexHexGrid -> TileKind
  getDieRollAt :: playerBoard -> Index HexHexGrid -> DieRoll

  isValidConfiguration :: playerBoard -> Map (Index HexHexGrid) Tile -> Bool
  isValidConfiguration pb = let
    grid = makeGrid pb
    f :: Index HexHexGrid -> Tile -> Bool -> Bool
    f i t = (&& (grid `contains` i) && (getKindAt pb i == getKind t))
    in Map.foldWithKey f True

-- * BasicPlayerBoard
data BasicPlayerBoard
  = BasicPlayerBoard
  deriving (Eq, Show, Read)
instance PlayerBoard BasicPlayerBoard where
  --                                     y:
  --                  6 5 4 3  ---------  3
  --                 2 1 6 5 4  --------  2
  --                5 4 3 1 2 3  -------  1
  --               6 1 2 6 5 4 1  ------  0
  --                2 5 4 3 1 2  ------- -1
  --             /   6 1 2 5 6  -------- -2
  --            / /   3 4 1 3  --------- -3
  --           / / /
  --          / / / / / / /
  --         / / / / / / /
  --   x:  -3-2-1 0 1 2 3
  getDieRollAt _ (-3, 0) = DieRoll6
  getDieRollAt _ (-3, 1) = DieRoll5
  getDieRollAt _ (-3, 2) = DieRoll2
  getDieRollAt _ (-3, 3) = DieRoll6

  getDieRollAt _ (-2,-1) = DieRoll2
  getDieRollAt _ (-2, 0) = DieRoll1
  getDieRollAt _ (-2, 1) = DieRoll4
  getDieRollAt _ (-2, 2) = DieRoll1
  getDieRollAt _ (-2, 3) = DieRoll5

  getDieRollAt _ (-1,-2) = DieRoll6
  getDieRollAt _ (-1,-1) = DieRoll5
  getDieRollAt _ (-1, 0) = DieRoll2
  getDieRollAt _ (-1, 1) = DieRoll3
  getDieRollAt _ (-1, 2) = DieRoll6
  getDieRollAt _ (-1, 3) = DieRoll4

  getDieRollAt _ ( 0,-3) = DieRoll3
  getDieRollAt _ ( 0,-2) = DieRoll1
  getDieRollAt _ ( 0,-1) = DieRoll4
  getDieRollAt _ ( 0, 0) = DieRoll6
  getDieRollAt _ ( 0, 1) = DieRoll1
  getDieRollAt _ ( 0, 2) = DieRoll5
  getDieRollAt _ ( 0, 3) = DieRoll3

  getDieRollAt _ ( 1,-3) = DieRoll4
  getDieRollAt _ ( 1,-2) = DieRoll2
  getDieRollAt _ ( 1,-1) = DieRoll3
  getDieRollAt _ ( 1, 0) = DieRoll5
  getDieRollAt _ ( 1, 1) = DieRoll2
  getDieRollAt _ ( 1, 2) = DieRoll4

  getDieRollAt _ ( 2,-3) = DieRoll1
  getDieRollAt _ ( 2,-2) = DieRoll5
  getDieRollAt _ ( 2,-1) = DieRoll1
  getDieRollAt _ ( 2, 0) = DieRoll4
  getDieRollAt _ ( 2, 1) = DieRoll3

  getDieRollAt _ ( 3,-3) = DieRoll3
  getDieRollAt _ ( 3,-2) = DieRoll6
  getDieRollAt _ ( 3,-1) = DieRoll2
  getDieRollAt _ ( 3, 0) = DieRoll1

  --                                     y:
  --                  G C C S  ---------  3
  --                 G G C S T  --------  2
  --                G G T S T T  -------  1
  --               W W W C W W W  ------  0
  --                T T M T T G  ------- -1
  --             /   T M S T T  -------- -2
  --            / /   M S S T  --------- -3
  --           / / /
  --          / / / / / / /
  --         / / / / / / /
  --   x:  -3-2-1 0 1 2 3
  getKindAt _ ( 0, 0) = CastleKind
  getKindAt _ (-2, 3) = CastleKind
  getKindAt _ (-1, 3) = CastleKind
  getKindAt _ (-1, 2) = CastleKind

  getKindAt _ ( _, 0) = ShipKind

  getKindAt _ ( 0, y) | y > 0     = ScienceKind
                      | otherwise = MineKind

  getKindAt _ ( 1,-2) = ScienceKind
  getKindAt _ ( 1,-3) = ScienceKind
  getKindAt _ ( 2,-3) = ScienceKind

  getKindAt _ ( 3,-1) = GrassKind
  getKindAt _ (-3, 1) = GrassKind
  getKindAt _ (-3, 2) = GrassKind
  getKindAt _ (-3, 3) = GrassKind
  getKindAt _ (-2, 1) = GrassKind
  getKindAt _ (-2, 2) = GrassKind

  getKindAt _ _       = TownKind

{-# LANGUAGE ExistentialQuantification #-}
module Game.Burgen.Board
  where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.List

import Game.Burgen.Types
import Math.Geometry.Grid
import Math.Geometry.Grid.Hexagonal

-- see https://github.com/mhwombat/grid/wiki/Hexagonal-tiles
-- see http://hackage.haskell.org/package/grid-7.8.8/docs/Math-Geometry-Grid-Hexagonal.html
-- see http://hackage.haskell.org/package/grid-7.8.8/docs/Math-Geometry-GridInternal.html#t:Grid

type BoardConfiguration
  = Map (Int,Int) Tile

--------------------------------------------------------------------------------
-- * Board

class Board board where
  getIndices :: board -> [(Int,Int)]
  getKindAt :: board -> (Int,Int) -> TileKind
  getDieRollAt :: board -> (Int,Int) -> DieRoll

  getInitialConfiguration :: board -> BoardConfiguration
  getInitialConfiguration _ = Map.fromList []

  getCurrentConfiguration :: board -> BoardConfiguration

--------------------------------------------------------------------------------
-- * PlayerBoardC and PlayerBoard

class (Eq playerBoard, Read playerBoard, Show playerBoard, Board playerBoard) =>
      PlayerBoardC playerBoard where

  isValidConfiguration :: playerBoard -> Bool
  isValidConfiguration pb = let
    f :: Index HexHexGrid -> Tile -> Bool -> Bool
    f i t = (&& (getKindAt pb i == getKind t))
    in Map.foldWithKey f True (getCurrentConfiguration pb)

data PlayerBoard
  = forall a. PlayerBoardC a => PlayerBoard a

instance Show PlayerBoard where
  show (PlayerBoard a) = show a

--------------------------------------------------------------------------------
-- * Player

data Player
  = Player { getPlayerName           :: UserId
           , getPlayerBoard          :: PlayerBoard
           , getCurrentPlayerDieRoll :: [DieRoll]
           , getCurrentPlayerScore   :: Int }
  deriving (Show)

instance Eq Player where
  p1 == p2 = (getPlayerName p1) == (getPlayerName p2)

--------------------------------------------------------------------------------
-- * CentralBoard

class (Eq centralBoard, Read centralBoard, Show centralBoard, Board centralBoard) =>
      CentralBoard centralBoard where
  sizeNumberOfBlackMarket :: centralBoard -> Int
  getCurrentBlackMarket :: centralBoard -> [Tile]
  getNumberOfPlayers :: centralBoard -> Int

  getCurrentColors :: centralBoard -> [Color]
  getPlayerByColor :: centralBoard -> Color -> Player


--------------------------------------------------------------------------------
-- * Basic instances

--------------------------------------------------------------------------------
-- ** BasicCentralBoard
data BasicCentralBoard
  = BasicCentralBoard BoardConfiguration -- ^ configuration
                      [Tile] -- ^ black market
                      Int -- ^ number of players in the Game
  deriving (Eq, Show, Read)

instance Board BasicCentralBoard where
  getInitialConfiguration _ = Map.fromList []
  getCurrentConfiguration (BasicCentralBoard bc _ _) = bc

  getIndices pn = let
    allIndices = [ [(1,0), (1,1), (1,2), (1,3)]
                 , [(2,0), (2,1), (2,2), (2,3)]
                 , [(3,0), (3,1), (3,2), (3,3)]
                 , [(4,0), (4,1), (4,2), (4,3)]
                 , [(5,0), (5,1), (5,2), (5,3)]
                 , [(6,0), (6,1), (6,2), (6,3)] ]
    indexReducer = take (getNumberOfPlayers pn)
    in concatMap indexReducer allIndices

  getKindAt _ (1,0) = GenKind
  getKindAt _ (1,1) = GenKind
  getKindAt _ (1,2) = GenKind
  getKindAt _ (1,3) = GenKind
  getKindAt _ (2,0) = GenKind
  getKindAt _ (2,1) = GenKind
  getKindAt _ (2,2) = GenKind
  getKindAt _ (2,3) = GenKind
  getKindAt _ (3,0) = GenKind
  getKindAt _ (3,1) = GenKind
  getKindAt _ (3,2) = GenKind
  getKindAt _ (3,3) = GenKind
  getKindAt _ (4,0) = GenKind
  getKindAt _ (4,1) = GenKind
  getKindAt _ (4,2) = GenKind
  getKindAt _ (4,3) = GenKind
  getKindAt _ (5,0) = GenKind
  getKindAt _ (5,1) = GenKind
  getKindAt _ (5,2) = GenKind
  getKindAt _ (5,3) = GenKind
  getKindAt _ (6,0) = GenKind
  getKindAt _ (6,1) = GenKind
  getKindAt _ (6,2) = GenKind
  getKindAt _ (6,3) = GenKind

  getDieRollAt _ (1,_) = DieRoll1 -- TODO: use Enum Instance
  getDieRollAt _ (2,_) = DieRoll2
  getDieRollAt _ (3,_) = DieRoll3
  getDieRollAt _ (4,_) = DieRoll4
  getDieRollAt _ (5,_) = DieRoll5
  getDieRollAt _ (6,_) = DieRoll6

instance CentralBoard BasicCentralBoard where
  getNumberOfPlayers (BasicCentralBoard _ _ pn) = pn
  sizeNumberOfBlackMarket (BasicCentralBoard _ _ 2) = 4
  sizeNumberOfBlackMarket (BasicCentralBoard _ _ 3) = 6
  sizeNumberOfBlackMarket (BasicCentralBoard _ _ 4) = 8
  getCurrentBlackMarket (BasicCentralBoard _ bm _) = bm

--------------------------------------------------------------------------------
-- ** BasicPlayerBoard
newtype BasicPlayerBoard
  = BasicPlayerBoard BoardConfiguration
  deriving (Eq, Show, Read)
instance Board BasicPlayerBoard where
  getInitialConfiguration _ = Map.fromList [((0,0), CastleTile)]
  getCurrentConfiguration (BasicPlayerBoard c) = c

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

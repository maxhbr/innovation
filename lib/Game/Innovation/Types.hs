module Game.Innovation.Types
    where
import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Basic types 
--------------------------------------------------------------------------------

data Color
  = Blue
  | Purple
  | Red
  | Yellow
  | Green
  deriving (Eq,Show,Enum,Ord)
colors :: [Color]
colors = map (\n -> toEnum n :: Color) [0..4]

data Age
  = Age1
  | Age2
  | Age3
  | Age4
  | Age5
  | Age6
  | Age7
  | Age8
  | Age9
  | Age10
  deriving (Eq,Show,Enum,Ord)
ages :: [Age]
ages = map (\n -> toEnum n :: Age) [0..9]

data Symbol
  = Castle
  | Tree
  | Coins
  | Bulb
  | Factory
  | Clock
  deriving (Eq,Show,Enum,Ord)

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

data Action
  = RawAction String
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------

data DogmaDescription
  = RawDescription String
  deriving (Eq,Show)

data Dogma
  = Dogma Symbol DogmaDescription
  | IDemand Symbol DogmaDescription
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Cards
--------------------------------------------------------------------------------

data Production
  = None
  | Produce Symbol
  deriving (Eq,Show)

--   +---------------+
--   | 1             |
--   |               |
--   | 2     3     4 |
--   +---------------+
data Productions
  = Productions { tlProduction :: Production
                , blProduction :: Production
                , bcProduction :: Production
                , brProduction :: Production }
  deriving (Eq,Show)

data Card
  = Card { color       :: Color
         , age         :: Age
         , productions :: Productions
         , dogmas      :: [Dogma] }
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- Players
--------------------------------------------------------------------------------

type Stack = [Card]

data SplayState
  = Left
  | Right
  | Up
  | NotSplayed
  deriving (Eq,Show)

type PlayerId = String
data Player
  = Player { playerId    :: PlayerId
           , stacks      :: Map Color Stack
           , splayStates :: Map Color SplayState
           , influence   :: Stack
           , dominations :: Stack
           , hand        :: Stack }
  deriving (Show)

mkPlayer :: PlayerId -> Player
mkPlayer playerId = Player playerId
                           (Map.fromList $ zip colors $ repeat [])
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           []
                           []
                           []

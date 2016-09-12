module Game.Innovation.Types
    where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Game.Innovation.Machine

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
  -- Basic / chooseable actions
  = Play Card
  | Draw
  | Dominate Age
  | Activate Color
  -- advanced actions
  | Archive
  | Recycle Card
  | Splay Color SplayState
  | DrawAnd Action
  | DrawFromAnd Age Action
  | Score Card
  | DrawFrom Age
  -- Raw
  | RawAction String
  deriving (Eq,Show)

drawAndDo :: (Card -> Action) -> Action
drawAndDo = undefined

drawFromAndDo :: (Card -> Action) -> Age -> Action
drawFromAndDo = undefined

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------

data Selector
  = Hand
  | Influence
  | StackOfColor Color
  | TheCard Card
  -- Selector combinators
  | OrSelector [Selector]
  | AndSelector [Selector]
  | OneOf Selector
  | HalfOf Selector
  | AllOf Selector
  | UpTo Selector
  -- Raw
  | RawSelector String
  deriving (Eq,Show)

data DogmaDescription
  = D Action
  | DD Action Selector
  -- DogmaDescription combinators
  | YouMay DogmaDescription
  | AndAlsoDo DogmaDescription DogmaDescription
  -- Raw
  | RawDescription String
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

data Player
  = Player { playerId    :: UserId
           , stacks      :: Map Color Stack
           , splayStates :: Map Color SplayState
           , influence   :: Stack
           , dominations :: Stack
           , hand        :: Stack }
  deriving (Show)

mkPlayer :: UserId -> Player
mkPlayer playerId = Player playerId
                           (Map.fromList $ zip colors $ repeat [])
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           []
                           []
                           []

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

data State
  = State { drawStacks   :: Map Age Stack
          , permutations :: Map Age [Int]
          , players      :: [Player]
          , history      :: Game State }
mkState :: Map Age [Int] -> State
mkState permutations = undefined


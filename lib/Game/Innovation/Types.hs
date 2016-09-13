{-# LANGUAGE FlexibleContexts #-}
module Game.Innovation.Types
    where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy

import Game.MetaGame

--------------------------------------------------------------------------------
-- Basic types
--------------------------------------------------------------------------------

data Color
  = Blue
  | Purple
  | Red
  | Yellow
  | Green
  deriving (Eq,Show,Read,Enum,Ord)
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
  deriving (Eq,Show,Read,Enum,Ord)
ages :: [Age]
ages = map (\n -> toEnum n :: Age) [0..9]

data Symbol
  = Castle
  | Tree
  | Coins
  | Bulb
  | Factory
  | Clock
  deriving (Eq,Show,Read,Enum,Ord)

--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

-- data Action
--   -- Basic / chooseable actions
--   = Play Card
--   | Draw
--   | Dominate Age
--   | Activate Color
--   -- advanced actions
--   | Archive
--   | Recycle Card
--   | Splay Color SplayState
--   | DrawAnd Action
--   | DrawFromAnd Age Action
--   | Score Card
--   | DrawFrom Age
--   -- Raw
--   | RawAction String
--   deriving (Eq,Show)
data Action
  =  RawAction String
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

type CardId = String
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
  = SplayedLeft
  | SplayedRight
  | SplayedUp
  | NotSplayed
  deriving (Eq,Show)

data Player
  = Player { getPlayerId    :: UserId
           , getStacks      :: Map Color Stack
           , getSplayStates :: Map Color SplayState
           , getInfluence   :: Stack
           , getDominations :: Stack
           , getHand        :: Stack }
  deriving (Show)

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

data Choices -- TODO

data State
  = Q0
  | Prepare State
  | State { getDrawStacks :: Map Age Stack
          , getPlayers    :: [Player]
          , getHistory    :: Game State }
  | WaitForChoices { choices :: [Choices]
                   , state   :: State }
  | FinishedGame State

pack :: UserActionC State action => action -> UserAction State
pack = pack' (Proxy :: Proxy State)

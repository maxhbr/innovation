module Game.Innovation.Dogmas
       where

import           Game.MetaGame
import           Game.Innovation.Types

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------

data Selector
  = Hand
  | Influence
  | StackOfColor Color
--   -- -| TheCard Card
--   -- Selector combinators
--   | OrSelector [Selector]
--   | AndSelector [Selector]
--   | OneOf Selector
--   | HalfOf Selector
--   | AllOf Selector
--   | UpTo Selector
  -- Raw
  | RawSelector String
  deriving (Eq,Show)

data DogmaDescription
  =
  --   D Action
  -- | DD Action Selector
  -- -- DogmaDescription combinators
  -- | YouMay DogmaDescription
  -- | AndAlsoDo DogmaDescription DogmaDescription
  -- Raw
    RawDescription String
  deriving (Eq,Show)

data Dogma
  = Dogma Symbol DogmaDescription
  | IDemand Symbol DogmaDescription
  deriving (Eq,Show)

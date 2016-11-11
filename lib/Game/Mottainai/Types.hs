{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Mottainai.Types
       ( module X
       , Material (..)
       , Effect (..)
       , Card (..)
       , Board

       , InnerMoveType
       , InnerMoveResult
       , OuterMoveResult
       , MoveType
       , MoveResult
       , MoveWR
       , Move
       , ActionType
       , ActionWR
       , Action
       , UserInput
       , does
       , chooses
       ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Proxy
import           System.Random
import           System.Random.Shuffle (shuffle')
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R

import qualified System.HsTColors as HsT

import qualified Game.MetaGame as MG
import           Game.MetaGame as X hiding ( InnerMoveType
                                           , InnerMoveResult
                                           , OuterMoveResult
                                           , MoveType
                                           , MoveResult
                                           , MoveWR
                                           , Action
                                           , Move
                                           , ActionType
                                           , ActionWR
                                           , UserInput
                                           , chooses
                                           , does
                                           )

--------------------------------------------------------------------------------
-- Basic types
--------------------------------------------------------------------------------

data Phase = Morning | Noon | Night
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Phase

data Material = Paper | Stone | Cloth | Clay | Metal
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Material
mkColored :: Material -> String -> String
mkColored Paper = HsT.mkBkMagenta
mkColored Stone = HsT.mkBkGreen . HsT.mkBlack
mkColored Cloth = HsT.mkBkYellow . HsT.mkBlack
mkColored Clay  = HsT.mkBkRed . HsT.mkBlack
mkColored Metal = HsT.mkBkBlue

materials :: [Material]
materials = [minBound ..]

data Task = Clerk | Monk | Tailor | Potter | Smith
          | Craft Material
          | Pray
  deriving (Eq,Show,Read,Ord)
instance View Task

data Value = One | Two | Three
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Value

values :: [Value]
values = [minBound ..]

materialToTask :: Material -> Task
materialToTask Paper = Clerk
materialToTask Stone = Monk
materialToTask Cloth = Tailor
materialToTask Clay  = Potter
materialToTask Metal = Smith

taskToMaterial :: Task -> Maybe Material
taskToMaterial Clerk     = Just Paper
taskToMaterial Monk      = Just Stone
taskToMaterial Tailor    = Just Cloth
taskToMaterial Potter    = Just Clay
taskToMaterial Smith     = Just Metal
taskToMaterial (Craft m) = Just m
taskToMaterial Pray      = Nothing

materialToValue :: Material -> Value
materialToValue Paper = One
materialToValue Stone = Two
materialToValue Cloth = Two
materialToValue Clay  = Three
materialToValue Metal = Three

--------------------------------------------------------------------------------
-- Effect
--------------------------------------------------------------------------------

data Effect = NoEffect
            | Effect String

--------------------------------------------------------------------------------
-- Cards
--------------------------------------------------------------------------------

data Card
  = Card { _title    :: String
         , _material :: Material
         , _effect   :: Effect }

instance Show Card where
  show Card{_title=title} = "["++title++"]"

instance View Card where
  view c@Card{ _material=mat } = ULogE Admin (T.pack $ "A Card")
                                             (T.pack $ mkColored mat (show c))

instance Eq Card where
  c1 == c2 = _title c1 == _title c2

--------------------------------------------------------------------------------
-- Players
--------------------------------------------------------------------------------
type RawStack = [Card]
class Stack a where
  getRawStack :: a -> RawStack
  setRawStack :: a -> RawStack -> a
  emptyStack :: a
instance Stack RawStack where
  getRawStack     = id
  setRawStack a _ = a
  emptyStack      = []

isEmptyStack :: Stack a =>
                a -> Bool
isEmptyStack = null . getRawStack

getStackSize :: Stack a =>
                a -> Int
getStackSize = length . getRawStack

onRawStack :: Stack a =>
              (RawStack -> RawStack) -> a -> a
onRawStack f a = setRawStack a (f (getRawStack a))

newtype DrawStack = DrawStack RawStack
                  deriving (Show)
instance Stack DrawStack where
  getRawStack (DrawStack ds) = ds
  setRawStack _ = DrawStack
  emptyStack = DrawStack []

newtype Hand = Hand RawStack
             deriving (Show)
instance Stack Hand where
  getRawStack (Hand is) = is
  setRawStack _ = Hand
  emptyStack = Hand []

instance View [Card] where
  view [] = fromString "[ 0 Cards ]"
  view cs = fromString ("[ " ++ show (length cs) ++ " Cards (head is " ++ show (head cs) ++ " ]")

data Player
  = Player { _playerId   :: UserId
           , _hand       :: Hand
           , _helpers    :: RawStack
           , _craftBench :: RawStack
           , _sales      :: RawStack
           , _gallery    :: RawStack
           , _giftShop   :: RawStack }

instance Eq Player where
  p1 == p2 = _playerId p1 == _playerId p2

instance PlayerC Player where
  getUId = _playerId


instance Show Player where
  show Player{_playerId=uid} = "[Player: " ++ show uid ++ "]"

instance View Player

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

type PlayerOrder = [UserId]

data Board = Board { _machineState :: MachineState
                   , _drawStack    :: DrawStack
                   , _floor        :: RawStack
                   , _players      :: [Player]
                   , _playerOrder  :: PlayerOrder
                   }
             deriving (Show)

type InnerMoveType = MG.InnerMoveType Board
type InnerMoveResult r = MG.InnerMoveResult Board r
type OuterMoveResult r = MG.OuterMoveResult Board r
type MoveType = MG.MoveType Board
type MoveResult r = MG.MoveResult Board r
type MoveWR r = MG.MoveWR Board r
type Move = MG.Move Board
type ActionType = MG.ActionType Board
type Action = MG.Action Board
type ActionWR r = MG.ActionWR Board r
type UserInput = MG.UserInput Board

does :: ActionToken Board actionToken =>
        UserId -> actionToken -> UserInput
does = MG.does (Proxy :: Proxy Board)

chooses :: UserId -> (UserId -> Answer) -> UserInput
chooses = MG.chooses (Proxy :: Proxy Board)

-- | possible answer (Yes)
toPlayMaybe :: UserId -> Answer
toPlayMaybe = answerYes

-- | possible answer (No)
notToPlayMaybe :: UserId -> Answer
notToPlayMaybe = answerNo

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

mkPlayer :: String -> Player
mkPlayer playerId = undefined

newtype Seed = Seed Int
             deriving (Show, Eq, Read)
instance View Seed where
  showUnrestricted _ = "[seed only visible for admin]"
  getOwner _ = Admin

-- | shuffle the draw stacks and the players
shuffleState :: Seed -> Board -> Board
shuffleState (Seed seed) board = board -- TODO

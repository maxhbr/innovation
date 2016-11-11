{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Innovation.Types
       where

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

import           Game.MetaGame

--------------------------------------------------------------------------------
-- Basic types
--------------------------------------------------------------------------------

data Phase = Morning | Noon | Night
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Phase

data Resource = Paper | Stone | Cloth | Clay | Metal
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Resuorce
mkColored :: Resuorce -> String -> String
mkColored Blue   = HsT.mkBkBlue
mkColored Purple = HsT.mkBkMagenta
mkColored Red    = HsT.mkBkRed . HsT.mkBlack
mkColored Yellow = HsT.mkBkYellow . HsT.mkBlack
mkColored Green  = HsT.mkBkGreen . HsT.mkBlack

resources :: [Resource]
resources = [minBound ..]

data Task = Clerk | Monk | Tailor | Potter | Smith
          | Craft Resuorce
          | Pray
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Task

tasks :: [Task]
tasks = [minBound ..]

data Value = One | Two | Three
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Value

values :: [Value]
values = [minBound ..]

resourceToTask :: Resource -> Task
resourceToTask Paper = Clerk
resourceToTask Stone = Monk
resourceToTask Cloth = Tailor
resourceToTask Clay  = Potter
resourceToTask Metal = Smith

taskToResource :: Task -> Maybe Resource
taskToResource Clerk   = Just Paper
taskToResource Monk    = Just Stone
taskToResource Tailor  = Just Cloth
taskToResource Potter  = Just Clay
taskToResource Smith   = Just Metal
taskToResource Craft m = Just m
taskToResource Pray    = Nothing

resourceToValue :: Resource -> Value
resourceToValue Paper = One
resourceToValue Stone = Two
resourceToValue Cloth = Two
resourceToValue Clay  = Three
resourceToValue Metal = Three

--------------------------------------------------------------------------------
-- Effect
--------------------------------------------------------------------------------

data Effect = Effect

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
  view c@Card{ _color=col } = ULogE Admin (T.pack $ "A Card")
                                          (T.pack $ mkColored col (show c))

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
  show p@(Player uid ps inf doms hand) = "[Player: " ++ show uid ++ "]"

instance View Player

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

type PlayerOrder = [UserId]

data Board = Board { _machineState        :: MachineState -- ^ The internal state of the underlying machine
                   , _drawStacks          :: Map Age DrawStack -- ^ the draw stacks, one for every age. The topmost card is the head
                   , _players             :: [Player] -- ^ the players playing in this game (in any order)
                   , _playerOrder         :: PlayerOrder -- ^ the order, in which the players take actions
                   }
             deriving (Show)

does :: ActionToken Board actionToken =>
        UserId -> actionToken -> UserInput Board
does = does' (Proxy :: Proxy Board)

chooses :: UserId -> (UserId -> Answer) -> UserInput Board
chooses = chooses' (Proxy :: Proxy Board)

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
mkPlayer playerId = Player (U playerId)
                           (Map.fromList $ zip colors $ repeat emptyStack)
                           emptyStack
                           (Dominations [])
                           emptyStack

newtype Seed = Seed Int
             deriving (Show, Eq, Read)
instance View Seed where
  showUnrestricted _ = "[seed only visible for admin]"
  getOwner _ = Admin

-- | shuffle the draw stacks and the players
shuffleState :: Seed -> Board -> Board
shuffleState (Seed seed) board = board{ _drawStacks=permutatedDS, _players=permutatedPlayers }
  where
    stdGen = mkStdGen seed
    shuffle []    = []
    shuffle list = shuffle' list (length list) stdGen
    permutatedDS = Map.map (onRawStack shuffle) (_drawStacks board)
    permutatedPlayers = shuffle $ _players board

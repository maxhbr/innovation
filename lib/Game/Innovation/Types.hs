{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.Innovation.Types
       where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import           Data.Proxy
import           System.Random
import           System.Random.Shuffle (shuffle')
import qualified Control.Lens as L
import           Data.Monoid
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S

import           Game.MetaGame

--------------------------------------------------------------------------------
-- Basic types
--------------------------------------------------------------------------------

data Color = Blue | Purple | Red | Yellow | Green
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Color

colors :: [Color]
colors = [minBound ..]

data Age = Age1 | Age2 | Age3 | Age4 | Age5 | Age6 | Age7 | Age8 | Age9 | Age10
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Age

ages :: [Age]
ages = [minBound ..]

data Symbol = Castle | Tree | Crown | Bulb | Factory | Clock
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Symbol

symbols :: [Symbol]
symbols = [minBound ..]

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------

data Dogma
  = Dogma Symbol String (Action Board)
  | IDemand Symbol String (Action Board)
instance Show Dogma where
  show (Dogma s d _)   = "[" ++ show s ++ "] " ++ d
  show (IDemand s d _) = "[" ++ show s ++ "] " ++ d

--------------------------------------------------------------------------------
-- Cards
--------------------------------------------------------------------------------

data Production
  = None
  | Produce { _prodSymbol :: Symbol }
  deriving (Eq,Show)

isSymbolProduction :: Production -> Bool
isSymbolProduction (Produce _) = True
isSymbolProduction _           = False


--   +----------------+
--   | tl             |
--   |                |
--   | bl    bc    br |
--   +----------------+
data Productions
  = Productions { _tlProd :: Production
                , _blProd :: Production
                , _bcProd :: Production
                , _brProd :: Production }
  deriving (Eq,Show)

data Card
  = Card { _title       :: String
         , _color       :: Color
         , _age         :: Age
         , _productions :: Productions
         , _dogmas      :: [Dogma] }
  | CardBackside Age
  deriving (Show)

data CardId = CardId { unpackCardId :: String }
            deriving (Eq, Show, Read)

instance Ord CardId where
  compare (CardId c1) (CardId c2) = compare c1 c2

getCId :: Card -> CardId
getCId Card{ _title=t, _age=a } = CardId $ "[" ++ show a ++ ": " ++ t ++ "]"

instance Eq Card where
  c1 == c2 = getCId c1 == getCId c2

instance Ord Card where
  compare c1 c2 = compare (getCId c1) (getCId c2)

data SpecialAchievement
  = SpecialAchievement { _achievementTitle :: String
                       , _achievementDescription :: String
                       , _achievementCondition :: Player -> Bool -- ^ programattically implementation of '_achievementDescription'
                       , _achievementAlternative :: String }

instance Show SpecialAchievement where
  show (SpecialAchievement t d _ a) = t ++ ": " ++ d ++ " (" ++ a ++ ")"

type SpecialAchievements = [SpecialAchievement]

getSAId :: SpecialAchievement -> CardId
getSAId SpecialAchievement{ _achievementTitle = t} = CardId $ "[" ++ t ++ "]"

instance Eq SpecialAchievement where
  sa1 == sa2 = getSAId sa1 == getSAId sa2

instance Ord SpecialAchievement where
  compare sa1 sa2 = compare (getSAId sa1) (getSAId sa2)

data Domination
  = AgeDomination Card
  | SpecialDomination SpecialAchievement
  deriving (Show)

instance View [Domination] where
  view [] = return $ T.pack "No dominations"
  view ds = return $ T.pack $ show (length ds) ++ " dominations"

--------------------------------------------------------------------------------
-- Players
--------------------------------------------------------------------------------
type RawStack = [Card]
class Stack a where
  getRawStack :: a -> RawStack
  setRawStack :: a -> RawStack -> a
  emptyStack :: a

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

data PlayStack = PlayStack RawStack SplayState
               deriving (Show)
instance Stack PlayStack where
  getRawStack (PlayStack ps _) = ps
  setRawStack (PlayStack _ ss) ps = PlayStack ps (if length ps > 1
                                                  then ss
                                                  else NotSplayed)
  emptyStack = PlayStack [] NotSplayed

getSplayState :: PlayStack -> SplayState
getSplayState (PlayStack _ ss) = ss

newtype Influence = Influence RawStack
                  deriving (Show)
instance Stack Influence where
  getRawStack (Influence is) = is
  setRawStack _ = Influence
  emptyStack = Influence []

newtype Hand = Hand RawStack
             deriving (Show)
instance Stack Hand where
  getRawStack (Hand is) = is
  setRawStack _ = Hand
  emptyStack = Hand []

newtype Dominateables = Dominateables RawStack
                      deriving (Show)
instance Stack Dominateables where
  getRawStack (Dominateables is) = is
  setRawStack _ = Dominateables
  emptyStack = Dominateables []

newtype Dominations = Dominations [Domination]
                      deriving (Show)

instance View [Card] where
  view [] = (return . T.pack) "[ 0 Cards ]"
  view cs = (return . T.pack) ("[ " ++ show (length cs) ++ " Cards (head is " ++ show (head cs) ++ " ]")

data SplayState
  = SplayedLeft
  | SplayedRight
  | SplayedUp
  | NotSplayed
  deriving (Eq,Show,Enum,Bounded)

instance View SplayState

data Player
  = Player { _playerId    :: UserId
           , _playStacks  :: Map Color PlayStack
           , _influence   :: Influence
           , _dominations :: Dominations
           , _hand        :: Hand }
  deriving (Show)

instance Eq Player where
  p1 == p2 = _playerId p1 == _playerId p2

instance PlayerC Player where
  getUId = _playerId

instance View Player where
  extractOwner = Just . getUId
  view p = undefined
  -- pp p = "** Player: " ++ pp (_playerId p) ++ ":\n"
  --        ++ pp (_stacks p)
  --        ++ pp (_splayStates p)
  --        ++ "influence: " ++ pp (_influence p) ++ "\n"
  --        ++ "dominations: " ++ pp (_dominations p) ++ "\n"
  --        ++ "hand: " ++ pp (_hand p) ++ "\n"

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

type PlayerOrder = [UserId]

data Board = Board { _machineState        :: MachineState -- ^ The internal state of the underlying machine
                   , _drawStacks          :: Map Age DrawStack -- ^ the draw stacks, one for every age. The topmost card is the head
                   , _dominateables       :: Dominateables -- ^ the cards, which could be dominated
                   , _players             :: [Player] -- ^ the players playing in this game (in any order)
                   , _playerOrder         :: PlayerOrder -- ^ the order, in which the players take actions
                   , _specialAchievements :: SpecialAchievements
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

-- | shuffle the draw stacks and the players
shuffleState :: Int -> Board -> Board
shuffleState seed board = board{ _drawStacks=permutatedDS, _players=permutatedPlayers }
  where
    stdGen = mkStdGen seed
    shuffle []    = []
    shuffle list = shuffle' list (length list) stdGen
    permutatedDS = Map.map (onRawStack shuffle) (_drawStacks board)
    permutatedPlayers = shuffle $ _players board

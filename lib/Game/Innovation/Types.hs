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
instance PrettyPrint Color where
  pp = show

colors :: [Color]
colors = [minBound ..]

data Age = Age1 | Age2 | Age3 | Age4 | Age5 | Age6 | Age7 | Age8 | Age9 | Age10
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance PrettyPrint Age where
  pp = show

ages :: [Age]
ages = [minBound ..]

data Symbol = Castle | Tree | Crown | Bulb | Factory | Clock
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance PrettyPrint Symbol where
  pp = show

symbols :: [Symbol]
symbols = [minBound ..]

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------

data Dogma
  = Dogma Symbol String (Action Board)
  | IDemand Symbol String (Action Board)
instance Show Dogma where
  show (Dogma s d _)   = "[" ++ pp s ++ "] " ++ d
  show (IDemand s d _) = "[" ++ pp s ++ "] " ++ d

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
  -- | SpecialAchievement {...}
  deriving (Show)

data CardId = CardId { unpackCardId :: String }
            deriving (Eq, Show, Read)

instance Ord CardId where
  compare (CardId c1) (CardId c2) = compare c1 c2

getCId :: Card -> CardId
getCId Card{ _title=t, _age=a } = CardId $ "[" ++ show a ++ ": " ++ t ++ "]"

instance Eq Card where
  c1 == c2 = getCId c1 == getCId c2

instance PrettyPrint Card where
  pp = unpackCardId . getCId

instance Ord Card where
  compare c1 c2 = compare (getCId c1) (getCId c2)

--------------------------------------------------------------------------------
-- Players
--------------------------------------------------------------------------------

type Stack = [Card]
emptyStack = [] :: Stack

instance PrettyPrint [Card] where
  pp [] = "[ 0 Cards ]"
  pp cs = "[ " ++ show (length cs) ++ " Cards (head is " ++ pp (head cs) ++ " ]"


instance (PrettyPrint k, PrettyPrint v) =>
         PrettyPrint (Map k v) where
  pp = let
    f k a result = result ++ "" ++ pp k ++ ": " ++ pp a ++ "\n"
    in Map.foldWithKey f ""

data SplayState
  = SplayedLeft
  | SplayedRight
  | SplayedUp
  | NotSplayed
  deriving (Eq,Show,Enum,Bounded)

instance PrettyPrint SplayState where
  pp = show

data Player
  = Player { _playerId    :: UserId
           , _stacks      :: Map Color Stack
           , _splayStates :: Map Color SplayState
           , _influence   :: Stack
           , _dominations :: Stack
           , _hand        :: Stack }
  deriving (Show)

instance Eq Player where
  p1 == p2 = _playerId p1 == _playerId p2

instance PlayerC Player where
  getUId = _playerId

instance PrettyPrint Player where
  pp p = "** Player: " ++ pp (_playerId p) ++ ":\n"
         ++ pp (_stacks p)
         ++ pp (_splayStates p)
         ++ "influence: " ++ pp (_influence p) ++ "\n"
         ++ "dominations: " ++ pp (_dominations p) ++ "\n"
         ++ "hand: " ++ pp (_hand p) ++ "\n"

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

-- data Choice = HasToChoose UserId Selector
--             | HasChosen UserId Selector
--             deriving (Eq, Show)

data MachineState
  = Prepare
  | WaitForTurn
  -- | WaitForChoices [Choice]
  | FinishedGame GameResult
  deriving (Eq, Show)
instance PrettyPrint MachineState where
  pp = show

type PlayerOrder = [UserId]

data Board = Board { _machineState  :: MachineState -- ^ The internal state of the underlying machine
                   , _drawStacks    :: Map Age Stack -- ^ the draw stacks, one for every age. The topmost card is the head
                   , _dominateables :: Stack -- ^ the cards, which could be dominated
                   , _players       :: [Player] -- ^ the players playing in this game (in any order)
                   , _playerOrder   :: PlayerOrder -- ^ the order, in which the players take actions
                   }
             deriving (Show)

instance BoardC Board where
  emptyBoard = Board Prepare Map.empty [] [] []

  getCurrentPlayer' Board{ _machineState= (FinishedGame _)} = Admin
  getCurrentPlayer' Board{ _machineState= Prepare}          = Admin
  getCurrentPlayer' Board{ _playerOrder=[] }                = Admin
  getCurrentPlayer' Board{ _playerOrder=order }             = head order

  advancePlayerOrder b@Board{ _playerOrder = ps } = b{ _playerOrder = (advancePlayerOrder' ps) }
    where
      advancePlayerOrder' :: PlayerOrder -> PlayerOrder
      advancePlayerOrder' []                       = []
      advancePlayerOrder' [p]                      = [p]
      advancePlayerOrder' (p1:(p2:ps)) | p1 == p2  = p2:ps -- ^ one consumes actions as long as it is not the last one
                                       | otherwise = p2:ps ++ [p1,p1] -- ^ every player gets two actions

  doAtomicUpdate = determineWinner . doSpecialAchievments
    where
      doSpecialAchievments = id -- TODO
      determineWinner = id -- TODO

  getGameResult board = let
    ms = _machineState board
    in case ms of
      FinishedGame gr -> gr
      _               -> NoWinner

instance PrettyPrint Board where
  pp b = (replicate 60 '=') ++ "\n"
         ++ "Board at " ++ pp (_machineState b) ++ " and current player is " ++ pp (getCurrentPlayer' b) ++ ":\n"
         ++ pp (_drawStacks b)
         ++ "dominateable: " ++ pp (_dominateables b) ++ "\n"
         ++ concatMap pp (_players b)

does :: ActionToken Board actionToken =>
        UserId -> actionToken -> Turn Board
does = does' (Proxy :: Proxy Board)

getMachineState :: MoveWR Board MachineState
getMachineState = M $ S.gets _machineState

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

mkPlayer :: String -> Player
mkPlayer playerId = Player (U playerId)
                           (Map.fromList $ zip colors $ repeat emptyStack)
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           emptyStack
                           emptyStack
                           emptyStack

-- | shuffle the draw stacks and the players
shuffleState :: Int -> Board -> Board
shuffleState seed gs = gs{ _drawStacks=permutatedDS, _players=permutatedPlayers }
  where
    stdGen = mkStdGen seed
    shuffle []    = []
    shuffle list = shuffle' list (length list) stdGen
    permutatedDS = Map.map shuffle $ _drawStacks gs
    permutatedPlayers = shuffle $ _players gs

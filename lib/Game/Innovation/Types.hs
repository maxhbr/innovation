{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Game.Innovation.Types
       where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Proxy
import           System.Random
import           System.Random.Shuffle (shuffle')
import           Control.Lens (makeLenses, Lens, Lens')
import qualified Control.Lens as L

import           Game.MetaGame

class PrettyPrint a where
  pp' :: a -> String
  pp :: a -> IO ()
  pp = putStrLn . pp'

--------------------------------------------------------------------------------
-- Basic types
--------------------------------------------------------------------------------

data Color = Blue | Purple | Red | Yellow | Green
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance PrettyPrint Color where
  pp' = show

colors :: [Color]
colors = [minBound ..]

data Age = Age1 | Age2 | Age3 | Age4 | Age5 | Age6 | Age7 | Age8 | Age9 | Age10
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance PrettyPrint Age where
  pp' = show

ages :: [Age]
ages = [minBound ..]

data Symbol = Castle | Tree | Crown | Bulb | Factory | Clock
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance PrettyPrint Symbol where
  pp' = show

symbols :: [Symbol]
symbols = [minBound ..]

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------
data StackId = Hand UserId
             | PlayStack UserId Color
             | Influence UserId
             | Dominations UserId
             | DrawStack Age
             deriving (Eq, Show, Read)

data Selector
  = RawSelector String -- ^ the verbal formulation of an selector

  | TheStackOfPlayer UserId StackId
--  | TheStackWithCard CardId
--   -- -| TheCard Card
--   -- Selector combinators
--   | OrSelector [Selector]
--   | AndSelector [Selector]
--   | OneOf Selector
--   | HalfOf Selector
--   | AllOf Selector
--   | UpTo Selector
  deriving (Eq,Show)

data DogmaDescription
  = RawDescription String -- ^ the verbal formulation of an description

  -- DogmaDescription combinators
  | YouMay DogmaDescription
  | AndAlsoDo DogmaDescription DogmaDescription
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
  | Produce { _prodSymbol :: Symbol }
  deriving (Eq,Show)
makeLenses ''Production

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
makeLenses ''Productions

data Card
  = Card { _title       :: String
         , _color       :: Color
         , _age         :: Age
         , _productions :: Productions
         , _dogmaTexts  :: [String]
         , _dogmas      :: [Dogma] }
  deriving (Eq,Show)
makeLenses ''Card

data CardId = CardId { unpackCardId :: String }
            deriving (Eq, Show, Read)

instance IDAble CardId Card where
  getId Card{ _title=t, _age=a } = CardId $ "[" ++ show a ++ ": " ++ t ++ "]"

instance PrettyPrint Card where
  pp' = unpackCardId . getId
--------------------------------------------------------------------------------
-- Players
--------------------------------------------------------------------------------

type Stack = [Card]
emptyStack = [] :: Stack

data SplayState
  = SplayedLeft
  | SplayedRight
  | SplayedUp
  | NotSplayed
  deriving (Eq,Show,Enum,Bounded)

data Player
  = Player { _playerId    :: UserId
           , _stacks      :: Map Color Stack
           , _splayStates :: Map Color SplayState
           , _influence   :: Stack
           , _dominations :: Stack
           , _hand        :: Stack }
  deriving (Show)
makeLenses ''Player

instance Eq Player where
  p1 == p2 = _playerId p1 == _playerId p2

instance IDAble UserId Player where -- UserC Player where
  getId = _playerId

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

data Choice = HasToChoose UserId Selector
            | HasChosen UserId Selector
            deriving (Eq, Show)

data MachineState
  = Prepare
  | WaitForTurn
  | WaitForChoices [Choice]
  | FinishedGame GameResult
  deriving (Eq, Show)

type PlayerOrder = [UserId]

data Board = Board { _machineState  :: MachineState -- ^ The internal state of the underlying machine
                   , _drawStacks    :: Map Age Stack -- ^ the draw stacks, one for every age. The topmost card is the head
                   , _dominateables :: Stack -- ^ the cards, which could be dominated
                   , _players       :: [Player] -- ^ the players playing in this game (in any order)
                   , _playerOrder   :: PlayerOrder -- ^ the order, in which the players take actions
                   }
             deriving (Show)
makeLenses ''Board

instance BoardC Board where
  emptyBoard = Board Prepare Map.empty [] [] []

  getCurrentPlayer'  Board{ _playerOrder=[] }    = Admin
  getCurrentPlayer'  Board{ _playerOrder=order } = head order

  getPlayerById' = undefined 

  advancePlayerOrder = L.over playerOrder advancePlayerOrder'
    where
      advancePlayerOrder' :: PlayerOrder -> PlayerOrder
      advancePlayerOrder' []                       = []
      advancePlayerOrder' [p]                      = [p]
      advancePlayerOrder' (p1:(p2:ps)) | p1 == p2  = p2:ps
                                       | otherwise = p2:ps ++ [p1,p1]

  getGameResult board = let
    ms = _machineState board
    in case ms of
      FinishedGame gr -> gr
      _               -> NoWinner

does :: ActionToken Board actionToken =>
        UserId -> actionToken -> Turn Board
does = does' (Proxy :: Proxy Board)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

mkPlayer :: String -> Player
mkPlayer playerId = Player (U playerId)
                           (Map.fromList $ zip colors $ repeat [])
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           []
                           []
                           []

-- | shuffle the draw stacks and the players
shuffleState :: Int -> Board -> Board
shuffleState seed gs = L.over players shuffle gs{ _drawStacks=permutatedDS }
  where
    stdGen = mkStdGen seed
    shuffle []    = []
    shuffle list = shuffle' list (length list) stdGen
    permutatedDS = Map.map shuffle $ _drawStacks gs

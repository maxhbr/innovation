{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Innovation.Types.Core
       ( module X
       , Color (..), colors, mkColored
       , Age (..), ages
       , Symbol (..), symbols, SymbolProvider (..)
       , Production (..), isSymbolProduction, Productions (..)
       , DogmaWR (..), Dogma, getDSymbol, getDAction, getDDesc
       , DogmaChain (..), dogmasFromList
       , CardId (..), Card (..)
       , RawStack
       , Stack (..), isEmptyStack, onRawStack, removeFromStack
       , DrawStack (..)
       , PlayStack (..), SplayState (..), getSplayState
       , Dominateables (..)
       , Domination (..), Dominations (..), addDomination
       , Hand (..)
       , SpecialAchievements
       , Player (..), mkPlayer
       , getInfluence
       , PlayerOrder
       , Seed (..)
       ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
-- import           System.Random
-- import           System.Random.Shuffle (shuffle')
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R

import qualified System.HsTColors as HsT

import           Game.MetaGame as X hiding ( getObject, setObject, modifyObject
                                           , getIdFyObject, setIdFyObject, modifyIdFyObject )

--------------------------------------------------------------------------------
-- Basic types
--------------------------------------------------------------------------------

data Color = Blue
           | Purple
           | Red
           | Yellow
           | Green
  deriving (Eq,Show,Read,Enum,Ord,Bounded)
instance View Color
mkColored :: Color -> String -> String
mkColored Blue   = HsT.mkBkBlue
mkColored Purple = HsT.mkBkMagenta
mkColored Red    = HsT.mkBkRed . HsT.mkBlack
mkColored Yellow = HsT.mkBkYellow . HsT.mkBlack
mkColored Green  = HsT.mkBkGreen . HsT.mkBlack

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

class SymbolProvider a where
  getProvidedSymbols :: a -> [Symbol]
  providesSymbol :: a -> Symbol -> Bool
  providesSymbol a = (`elem` getProvidedSymbols a)

--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------

type WithInputAndOutput a b m = ReaderT a (WriterT b m)

data DogmaWR a b
  = Dogma Symbol Text Action
  | GenDogma Symbol Text (WithInputAndOutput a b ActionWR ())
  | IDemand Symbol Text (UserId -- ^ user issueing the dogma
                         -> Action)
  | GenIDemand Symbol Text (UserId -- ^ user issueing the dogma
                            -> WithInputAndOutput a b ActionWR ())
instance Show (DogmaWR a b) where
  show (Dogma s d _)      = "[" ++ show s ++ "] " ++ T.unpack d
  show (GenDogma s d _)   = "[" ++ show s ++ "] " ++ T.unpack d
  show (IDemand s d _)    = "[" ++ show s ++ "] " ++ T.unpack d
  show (GenIDemand s d _) = "[" ++ show s ++ "] " ++ T.unpack d
type Dogma = DogmaWR () ()

getDSymbol :: DogmaWR a b -> Symbol
getDSymbol (Dogma symb _ _)      = symb
getDSymbol (GenDogma symb _ _)   = symb
getDSymbol (IDemand symb _ _)    = symb
getDSymbol (GenIDemand symb _ _) = symb

getDDesc :: DogmaWR a b -> Text
getDDesc (Dogma _ text _)      = text
getDDesc (GenDogma _ text _)   = text
getDDesc (IDemand _ text _)    = text
getDDesc (GenIDemand _ text _) = text

getDAction :: Monoid b =>
              DogmaWR a b -> a -> ActionWR b
getDAction (Dogma _ _ act)      _ = act >> return mempty
getDAction (GenDogma _ _ act)   a = fmap snd (W.runWriterT (R.runReaderT act a))
getDAction (IDemand _ _ act)    _ = A R.ask >>= act >> return mempty
getDAction (GenIDemand _ _ act) a = A R.ask >>= (\uid -> fmap snd (W.runWriterT (R.runReaderT (act uid) a)))

data DogmaChain a c
  = EDogmaChain
  | forall b.
    Monoid b =>
    DogmaChain (DogmaWR a b) (DogmaChain b c)

type Dogmas = DogmaChain () ()

dogmasFromList :: [DogmaWR () ()] -> Dogmas
dogmasFromList = foldr DogmaChain EDogmaChain

--------------------------------------------------------------------------------
-- Cards
--------------------------------------------------------------------------------

data Production
  = None
  | Produce { _prodSymbol :: Symbol }
  deriving (Eq,Show)
instance SymbolProvider Production where
  getProvidedSymbols None        = []
  getProvidedSymbols (Produce s) = [s]

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
instance SymbolProvider Productions where
  getProvidedSymbols (Productions tl bl bc br) = concatMap getProvidedSymbols [tl, bl, bc ,br]

data CardId = CardId { unpackCardId :: String }
            deriving (Eq, Read)

instance Show CardId where
  show (CardId cardId) = cardId

instance View CardId

instance Ord CardId where
  compare (CardId c1) (CardId c2) = compare c1 c2

data Card
  = Card { _title       :: String
         , _color       :: Color
         , _age         :: Age
         , _productions :: Productions
         , _dogmas      :: Dogmas }
--  | CardId String
--  | CardBackside Age
type instance IdF Card = CardId
instance IdAble Card where
  idOf Card{ _title=t, _age=a } = CardId ("[" ++ show a ++ ": " ++ t ++ "]")

instance SymbolProvider Card where
  getProvidedSymbols Card{_productions=productions} = getProvidedSymbols productions

instance SymbolProvider [Card] where
  getProvidedSymbols = concatMap getProvidedSymbols

-- toBackside :: Card -> Card
-- toBackside Card{ _age=a } = CardBackside a
-- toBackside c              = c

instance Show Card where
  show = show . idOf
  -- show (CardBackside a)         = "[card of " ++ show a ++ "]"

instance View Card where
  view c@Card{ _color=col } = ULogE Admin (T.pack $ "[Card of " ++ show (_age c) ++"]")
                                          (T.pack $ mkColored col (show c))
  -- view (CardBackside a)     = fromString ("[card of " ++ show a ++ "]")

instance Eq Card where
  c1 == c2 = idOf c1 == idOf c2

instance Ord Card where
  compare c1 c2 = compare (idOf c1) (idOf c2)

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
  view [] = fromString "No dominations"
  view ds = fromString (show (length ds) ++ " dominations")

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

removeFromStack :: Stack a =>
                   Card -> a -> a
removeFromStack c = onRawStack (filter (/= c))

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

addDomination :: Domination -> Dominations -> Dominations
addDomination d (Dominations ds) = Dominations (d:ds)

instance View [Card] where
  view [] = fromString "[ 0 Cards ]"
  view cs = fromString ("[ " ++ show (length cs) ++ " Cards (head is " ++ show (head cs) ++ " ]")

data SplayState
  = SplayedLeft
  | SplayedRight
  | SplayedUp
  | NotSplayed
  deriving (Eq,Show,Enum,Bounded)

instance View SplayState

data Player
  = Player { _playerId    :: UserId
           , _zone        :: Map Color PlayStack
           , _influence   :: Influence
           , _dominations :: Dominations
           , _hand        :: Hand }
type instance IdF Player = UserId
instance IdAble Player where
  idOf Player{_playerId=userId} = userId

instance Eq Player where
  p1 == p2 = _playerId p1 == _playerId p2
instance PlayerC Player

getInfluence :: Player -> Int
getInfluence (Player _ _ (Influence is) _ _) = sum (map (fromEnum . _age) is)

getDominationCount :: Player -> Int
getDominationCount (Player _ _ _ (Dominations ds) _) = length ds

instance Show Player where
  show p@(Player uid ps inf doms hand) = let
    influence = show (getInfluence p)
    numOfDominations = show (getDominationCount p)
    numOfHandCards = show (getStackSize hand)
    in "[Player: " ++ show uid
       ++ " handCards: " ++ numOfHandCards
       ++ " infl: " ++ influence
       ++ " doms: " ++ numOfDominations ++ "]"

instance View Player where
  getOwner = idOf
  view = view . _playerId

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

type PlayerOrder = [UserId]

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

-- -- | shuffle the draw stacks and the players
-- shuffleState :: Seed -> Board -> Board
-- shuffleState (Seed seed) board = board{ _drawStacks=permutatedDS, _players=permutatedPlayers }
--   where
--     stdGen = mkStdGen seed
--     shuffle :: [a] -> [a]
--     shuffle []    = []
--     shuffle list = shuffle' list (length list) stdGen
--     permutatedDS = Map.map (onRawStack shuffle) (_drawStacks board)
--     permutatedPlayers = shuffle $ _players board

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Game.Innovation.BaseTypes

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
  = Productions { _tlProd :: Production -- top left
                , _blProd :: Production -- bottom left
                , _bcProd :: Production -- bottom center
                , _brProd :: Production -- bottom right
                }
  deriving (Eq,Show)
makeLenses ''Productions

class CardC card where
  cTitle       :: card -> String
  cColor       :: card -> Color
  cAge         :: card -> Age
  cproductions :: card -> Productions
  cDogmas      :: card -> [Dogma]

data Card = forall card.
            CardC card =>
            Card { getCard :: card }

instance CardC Card where
  cTitle       (Card card) = cTitle       card
  cColor       (Card card) = cColor       card
  cAge         (Card card) = cAge         card
  cproductions (Card card) = cproductions card
  cDogmas      (Card card) = cDogmas      card

instance Show Card where
  show (Card card) = cTitle card

data CardId = CardId String
            deriving (Eq, Show, Read)

instance IDAble CardId Card where
  getId card = CardId $ cAge card ++ ":" ++ cTitle card :: CardId

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

data Selector
  deriving Show

data Choice
  = HasToChoose UserId Selector
  | HasChosen UserId Selector
  deriving (Show)

-- | The state of the underlying machine, which determines who has to act next
-- and which actions are possible
data MachineState
  =
    -- | Prepare means, that the game has not yet startet,
    -- i.e. the Admin should take actions
    Prepare
    -- | WaitForTurn means, that the player currently first in the playerOrder
    -- should take his turn
  | WaitForTurn
    -- | WaitForChoices means, that a previous action, which is not yet done,
    -- needs some player to choose cards, colors, stacks, ...
  | WaitForChoices [Choice]
    -- | FinishedGame means, that the game has ended with some gameResult,
    -- no action should modify this state
  | FinishedGame GameResult
  deriving (Eq, Show)

-- | The State contains the complete State of the game including
--
--    - all card stacks
--    - the state of the underlying machine (includes the game result!)
--    - the playerOrder
--    - the history
data State = State { _machineState :: MachineState
                   , _drawStacks   :: Map Age Stack
                   , _players      :: [Player]
                   , _playerOrder  :: PlayerOrder
                   , _history      :: Game State }
makeLenses ''State

instance StateC State where
  emptyState = State Prepare Map.empty [] [] (G [])

  getCurrentPlayer'  State{ _playerOrder=[] }    = Admin
  getCurrentPlayer'  State{ _playerOrder=order } = head order

  advancePlayerOrder = L.over playerOrder advancePlayerOrder'
    where
      advancePlayerOrder' :: PlayerOrder -> PlayerOrder
      advancePlayerOrder' []                       = []
      advancePlayerOrder' [p]                      = [p]
      advancePlayerOrder' (p1:(p2:ps)) | p1 == p2  = p2:ps
                                       | otherwise = p2:ps ++ [p1,p1]

  getGameResult state = let
    ms = _machineState state
    in case ms of
      FinishedGame gr -> gr
      _               -> NoWinner

-- | this allows us to write thins like 'Admin `does` AddPlayer "player1"'
does :: ActionC State actionToken =>
        UserId -> actionToken -> Action State
does = does' (Proxy :: Proxy State)

--------------------------------------------------------------------------------
-- Generators and helper
--------------------------------------------------------------------------------

mkPlayer :: String -> Player
mkPlayer playerId = Player (U playerId)
                           (Map.fromList $ zip colors $ repeat [])
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           []
                           []
                           []

shuffleState :: Int -> State -> State
shuffleState seed gs = gs{ _drawStacks=permutatedDS }
  where
    stdGen = mkStdGen seed
    shuffle []    = []
    shuffle stack = shuffle' stack (length stack) stdGen
    permutatedDS = Map.map shuffle $ _drawStacks gs


--------------------------------------------------------------------------------
-- Dogmas
--------------------------------------------------------------------------------
data DogmaDescription
  =
    -- | every affected player has to do the described action
    D (Action State) -- TODO: needs better name
    -- | the affected player has the choice to do the described action
  | YouMay DogmaDescription
  -- Raw
    -- | the description was not yet implemented and the part to implement is
    -- given by the 'String'
  | RawDescription String
  deriving (Eq,Show)

data Dogma
  =
    -- | Describes a cooperative dogma based on the given symbol
    Dogma Symbol DogmaDescription
    -- | Describes a aggresive dogma based on the given symbol
  | IDemand Symbol DogmaDescription
  deriving (Eq,Show)

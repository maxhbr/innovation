{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Rules
    where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import Data.Proxy

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Cards

instance IsUserable State where
  getCurrentUser (FinishedGame state) = getCurrentUser state
  getCurrentUser _                    = undefined

#if false
pack :: UserActionC State action => action -> UserAction State
pack = pack' (Proxy :: State)
#endif

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

mkPlayer :: UserId -> Player
mkPlayer playerId = Player playerId
                           (Map.fromList $ zip colors $ repeat [])
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           []
                           []
                           []

mkPermutations :: Map Age [Int]
mkPermutations = undefined

mkInitialState :: Map Age [Int] -> State
mkInitialState permutations = State permutatedDrawStack [] []
  where
    permutatedDrawStack = Map.mapWithKey permutate permutations
      where
        permutate :: Age -> [Int] -> Stack
        permutate age (i:is) = let
          agethStack = fromJust $ Map.lookup age cards
          ithCard = agethStack !! i
          in ithCard : permutate age is
        permutate age []     = []

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getCurrentPlayer :: State -> Player
getCurrentPlayer = undefined

getCurrentAge :: Player -> Age
getCurrentAge player = let
  currentAges = (map (fromEnum . age . head) . filter (not . null) . Map.elems) $ stacks player
  in if currentAges /= []
     then toEnum $ maximum currentAges
     else Age1

getCurrentDrawAge :: Player -> State -> Maybe Age
getCurrentDrawAge player state = if null agesAboveWithCards
                                 then Nothing
                                 else Just $ head agesAboveWithCards
  where
    currentAge         = getCurrentAge player
    drawStacks         = getDrawStacks state
    agesAboveWithCards = Map.keys $ Map.filterWithKey (\ age stack -> age >= currentAge && stack /= []) drawStacks

--------------------------------------------------------------------------------
-- Chooseable actions
--------------------------------------------------------------------------------
--   = Play Card
--   | Draw
--   | Dominate Age
--   | Activate Color
data Draw = Draw
            deriving (Read, Show)

instance UserActionC State Draw where
  getTransition' _ state = W.writer (Right undefined, logs)
    where
      currentPlayer  = getCurrentPlayer state
      currentDrawAge = getCurrentDrawAge currentPlayer state
      logs           = undefined

#if false
chooseableActions :: [UserAction State]
chooseableActions = map pack [Draw]
#endif

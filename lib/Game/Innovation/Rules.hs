{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Rules
    where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Data.Proxy
import           Control.Lens

import           Game.MetaGame
import           Game.Innovation.Types
import           Game.Innovation.Cards

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getStackFromMapBy :: (Ord k) => k -> Map k Stack -> Stack
getStackFromMapBy = Map.findWithDefault emptyStack

getPlayerByUserId :: Maybe UserId -> State -> Maybe Player
getPlayerByUserId Nothing _           = Nothing
getPlayerByUserId (Just userId) state = undefined

--------------------------------------------------------------------------------
-- Getter for ages
--------------------------------------------------------------------------------

getCurrentAge :: Player -> Age
getCurrentAge player = let
  currentAges = (map (fromEnum . view age . head) . filter (not . null) . Map.elems) $ _stacks player
  in if currentAges /= []
     then toEnum $ maximum currentAges
     else Age1

getCurrentDrawAge :: Player -> State -> Maybe Age
getCurrentDrawAge player state = if null agesAboveWithCards
                                 then Nothing
                                 else Just $ head agesAboveWithCards
  where
    currentAge         = getCurrentAge player
    currentDrawStacks  = view drawStacks state
    agesAboveWithCards = Map.keys $
                         Map.filterWithKey (\ age stack -> age >= currentAge
                                                        && stack /= []) currentDrawStacks

--------------------------------------------------------------------------------
-- Getter for visible productions and related symbols
--------------------------------------------------------------------------------

getSymbols :: Player -> Map Symbol Int
getSymbols player = Map.fromListWith (+) $
                    zip playersSymbolsList (repeat 1)
  where
    playersSymbolsList = map _prodSymbol $
                         filter isSymbolProduction $
                         getProductions player

getProductions :: Player -> [Production]
getProductions player = concatMap (`getProductionsForColor` player) colors

getProductionsForColor :: Color -> Player -> [Production]
getProductionsForColor color player = getProductionsForStack stackOfColor splayStateOfColor
  where
    stackOfColor      = getStackFromMapBy color $ view stacks player
    splayStateOfColor = Map.findWithDefault NotSplayed color $ view splayStates player

getProductionsForStack :: Stack -> SplayState -> [Production]
getProductionsForStack [] _              = []
getProductionsForStack [card] _          = map ((\v -> v (view productions card)) . view)
                                               [ tlProd, blProd, bcProd, brProd ]
getProductionsForStack (c:cs) splayState = getProductionsForStack [c] splayState ++ prodOfInactive
  where
    prodOfInactive = concatMap (\card -> map ((\v -> v (view productions card)) . view) $
                                         getLenses splayState) cs
    getLenses SplayedLeft  = [ brProd ]
    getLenses SplayedRight = [ tlProd, blProd ]
    getLenses SplayedUp    = [ blProd, bcProd, brProd ]
    getLenses NotSplayed   = []

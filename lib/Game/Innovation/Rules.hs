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

advancePlayerOrder :: PlayerOrder -> PlayerOrder
advancePlayerOrder []                       = []
advancePlayerOrder [p]                      = [p]
advancePlayerOrder (p1:(p2:ps)) | p1 == p2  = p2:ps
                                | otherwise = p2:ps ++ [p1,p1]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getPlayerByUserId :: Maybe UserId -> State -> Maybe Player
getPlayerByUserId Nothing _           = Nothing
getPlayerByUserId (Just userId) state = undefined

getCurrentAge :: Player -> Age
getCurrentAge player = let
  currentAges = (map (fromEnum . age . head) . filter (not . null) . Map.elems) $ getStacks player
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

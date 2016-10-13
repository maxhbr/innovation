{-# LANGUAGE LambdaCase #-}
module Game.Innovation.TestHelper
       where

import SpecHelper

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Game.Innovation.Types
import Game.Innovation.Cards

getAllCardsFromMap :: Stack a =>
                      Map k a -> RawStack
getAllCardsFromMap = Map.foldr (++) [] . Map.map getRawStack

getAllCurrentCards :: Board -> RawStack
getAllCurrentCards (Board _ drawStacks dominateables players _ _) = cardsInDrawStacks ++ (getRawStack dominateables) ++ cardsAtPlayers
  where
    cardsInDrawStacks = getAllCardsFromMap drawStacks
    cardsAtPlayers    = concatMap getAllCardsOfPlayer players
    getAllCardsOfPlayer :: Player -> RawStack
    getAllCardsOfPlayer (Player _ stacks influence (Dominations ds) hand) = getAllCardsFromMap stacks ++ (getRawStack influence) ++ dominationCards ++ (getRawStack hand)
      where
        dominationCards = concatMap (\case
                                        AgeDomination c -> [c]
                                        _               -> []) ds

exactlyAllCardsArePresent :: Board -> Bool
exactlyAllCardsArePresent board = noCardsAreDuplicates && allCardsArePresent
  where
    allCurrentCards      = sort $ getAllCurrentCards board
    allStartingCards     = sort getCards
    noCardsAreDuplicates = allCurrentCards == nub allCurrentCards
    allCardsArePresent   = allCurrentCards == allStartingCards


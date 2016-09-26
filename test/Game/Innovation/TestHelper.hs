module Game.Innovation.TestHelper
       where

import SpecHelper

import Data.Map (Map)
import qualified Data.Map as Map

import Game.Innovation.Types
import Game.Innovation.Cards

getAllCardsFromMap :: Map a Stack -> Stack
getAllCardsFromMap = Map.foldr (++) []

getAllCurrentCards :: Board -> Stack
getAllCurrentCards (Board _ drawStacks dominateables players _) = cardsInDrawStacks ++ dominateables ++ cardsAtPlayers
  where
    cardsInDrawStacks = getAllCardsFromMap drawStacks
    cardsAtPlayers    = concatMap getAllCardsOfPlayer players
    getAllCardsOfPlayer :: Player -> Stack
    getAllCardsOfPlayer (Player _ stacks _ influence dominations hand) = undefined

getAllStartingCards :: Stack
getAllStartingCards = getAllCardsFromMap getDeck

exactlyAllCardsArePresent :: Board -> Bool
exactlyAllCardsArePresent state = undefined
  where
    noCardsAreDuplicates = undefined
    allCardsArePresent   = undefined


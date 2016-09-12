module Game.Innovation.TestHelper
       where

import SpecHelper

import Data.Map (Map)
import qualified Data.Map as Map

import Game.Innovation.Types
import Game.Innovation.Cards

getAllCardsFromMap :: Map a Stack -> Stack
getAllCardsFromMap = Map.foldr (++) []

getAllCurrentCards :: State -> Stack
getAllCurrentCards (State drawStacks _ players _) = cardsInDrawStacks ++ cardsAtPlayers
  where
    cardsInDrawStacks = getAllCardsFromMap drawStacks
    cardsAtPlayers    = flatMap getAllCardsOfPlayer players
    getAllCardsOfPlayer :: Player -> Stack
    getAllCardsOfPlayer (Player _ stacks _ influence dominations hand) = undefined

getAllStartingCards :: Stack
getAllStartingCards = getAllCardsFromMap cards

exactlyAllCardsArePresent :: State -> Boolean
exactlyAllCardsArePresent state = undefined
  where
    noCardsAreDuplicates = undefined
    allCardsArePresent   = undefined


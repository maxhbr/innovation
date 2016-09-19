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
getAllCurrentCards (State _ drawStacks players _ _) = cardsInDrawStacks ++ cardsAtPlayers
  where
    cardsInDrawStacks = getAllCardsFromMap drawStacks
    cardsAtPlayers    = concatMap getAllCardsOfPlayer players
    getAllCardsOfPlayer :: Player -> Stack
    getAllCardsOfPlayer (Player _ stacks _ influence dominations hand) = undefined

getAllStartingCards :: Stack
getAllStartingCards = getAllCardsFromMap cards

exactlyAllCardsArePresent :: State -> Bool
exactlyAllCardsArePresent state = undefined
  where
    noCardsAreDuplicates = undefined
    allCardsArePresent   = undefined


module Game.Innovation.TestHelper
       where

import SpecHelper

import Data.List
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
    getAllCardsOfPlayer (Player _ stacks _ influence dominations hand) = getAllCardsFromMap stacks ++ influence ++ dominations ++ hand

exactlyAllCardsArePresent :: Board -> Bool
exactlyAllCardsArePresent board = noCardsAreDuplicates && allCardsArePresent
  where
    allCurrentCards      = sort $ getAllCurrentCards board
    allStartingCards     = sort getCards
    noCardsAreDuplicates = allCurrentCards == nub allCurrentCards
    allCardsArePresent   = allCurrentCards == allStartingCards


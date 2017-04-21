{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.Innovation.ActionTokens
       ( AddPlayer (..), StartGame (..)
       , Draw (..), Play (..), Dominate (..), Activate (..)
       ) where

import           Prelude hiding (log)
import           Control.Monad

import           Game.Innovation.Types
import qualified Game.Innovation.Cards as Cards
import           Game.Innovation.Rules

onlyPrepareState :: MoveType Bool
onlyPrepareState = do
  ms <- getMachineState
  return (ms == Prepare)

--------------------------------------------------------------------------------
-- * Admin actions

-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer String
               deriving (Eq, Show, Read)
instance View AddPlayer
instance ActionToken AddPlayer where
  stateMatchesExpectation _ = onlyPrepareState

  getAction (AddPlayer newPlayerId) = (mkAdminA . addPlayer) newPlayerId

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame Int
               deriving (Eq, Show, Read)
instance View StartGame where
  view (StartGame seed) = "StartGame " <<> view (Seed seed)
instance ActionToken StartGame where
  stateMatchesExpectation _ = onlyPrepareState

  getAction (StartGame seed) = mkAdminA $ do
    M $ log "start the game"
    M $ do
      ps <- getPlayers
      unless (length ps >= 2 && length ps <= 4)
        (logError "Numer of players is not valid")
    setDeck Cards.getDeck
    shuffle (Seed seed)
    drawDominations
    handOutInitialCards

-- data DropPlayer = DropPlayer UserId
--                 deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- * Player actions

-- | Draw
data Draw = Draw
          deriving (Eq, Read, Show)
instance View Draw
instance ActionToken Draw where
  getAction Draw = drawAnd >>= putIntoHand

-- | Play
data Play = Play CardId
          deriving (Eq, Read, Show)
instance View Play where
  view (Play cardId) = "Play " <<> view cardId
instance ActionToken Play where
  getAction (Play cardId) = putTheHandCardsIntoPlay [cardId]

-- | Dominate
data Dominate = Dominate Age
              deriving (Eq, Read, Show)
instance View Dominate
instance ActionToken Dominate where
  getAction (Dominate age) = dominateAge age

-- | Activate
data Activate = Activate Color
              deriving (Eq, Read, Show)
instance View Activate
instance ActionToken Activate where
  getAction (Activate color) = activate color

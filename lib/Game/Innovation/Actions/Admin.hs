{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Innovation.Actions.Admin
    ( Init (..)
    , Shuffle (..)
    , DrawDominations (..)
    , AddPlayer (..)
    , StartGame (..)
    ) where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Monoid
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Control.Lens

import           Game.MetaGame
import           Game.Innovation.Types
import           Game.Innovation.TypesLenses
import           Game.Innovation.Cards
import           Game.Innovation.Rules

--------------------------------------------------------------------------------
-- Admin actions
--------------------------------------------------------------------------------
doesInSequence = undefined

-- | Init
data Init = Init
          deriving (Eq, Show, Read)
instance ActionToken Board Init where
  getAction Init = onlyAdminIsAllowed $
                   mkA $ \userId -> do
                     log "Init the board"
                     S.modify (\board -> board{ _drawStacks=getDeck })

-- | Shuffle
-- create an empty game using a given seed
data Shuffle = Shuffle Int
             deriving (Eq, Show, Read)
instance ActionToken Board Shuffle where
  getAction (Shuffle seed) = onlyAdminIsAllowed $
                             mkA $ \userId -> do
                               logForMe ("Shuffle with seed [" ++ show seed ++ "]")
                                 "Shuffle with seed [only visible for admin]"
                               S.modify (shuffleState seed)

data DrawDominations = DrawDominations
                     deriving (Eq, Show, Read)
instance ActionToken Board DrawDominations where
  getAction DrawDominations = onlyAdminIsAllowed $
                              mkA $ \userId -> do
                                log "Draw dominations"
                                S.modify id -- TODO

-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer String
               deriving (Eq, Show, Read)
instance ActionToken Board AddPlayer where
  getAction (AddPlayer playerId) = onlyAdminIsAllowed $
                                   mkA $ \userId -> do
                                     log ("Add player: " ++ playerId)
                                     state <- S.get
                                     case view machineState state of
                                       Prepare -> do
                                         let newPlayer = mkPlayer playerId
                                         S.modify (const (players %~ (newPlayer :) $
                                                          state))
                                       _       -> logError "not in prepare state."

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame Int
               deriving (Eq, Show, Read)
instance ActionToken Board StartGame where
  getAction (StartGame seed) = onlyAdminIsAllowed $
                               A $ \userId ->
    (turnToMove $ userId `does` Shuffle seed) <>
    (turnToMove $ userId `does` DrawDominations) <>
    (M ( do
            log "Start game"
            ps <- use players
            if length ps >= 2 && length ps <=4
              then do
              -- Ask for first card
              S.modify id -- TODO
              else logError "Numer of players is not valid"
       )) <>
    -- ... wait..
    (M ( do
            -- play chosen cards
            -- determine starting player
            ps <- use players
            playerOrder .= map getUId ps -- FALSE!
            machineState .= WaitForTurn
       ))

data DropPlayer = DropPlayer UserId
                deriving (Eq, Show, Read)

data Undo = Undo
          deriving (Eq, Show, Read)

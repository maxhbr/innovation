{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Innovation.Actions.Admin
    ( Init (..)
    , SetCardDeck (..)
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
import           Game.Innovation.Cards
import           Game.Innovation.Rules

--------------------------------------------------------------------------------
-- Admin actions
--------------------------------------------------------------------------------

-- | Init
-- Does:
--  - SetCardDeck
--  - Shuffle
--  - DrawDominations
data Init = Init DeckName Int
          deriving (Show, Read)
instance ActionC State Init where
  toTransition' userId (Init deckName seed) =
    userId `onlyAdminIsAllowed`
    undefined
    -- T ((toTransition' userId $ SetCardDeck deckName) >>=
    --    (toTransition' userId $ Shuffle seed) >>=
    --    (toTransition' userId DrawDominations))

-- | SetCardDeck
data SetCardDeck = SetCardDeck DeckName
                 deriving (Show, Read)
instance ActionC State SetCardDeck where
  toTransition' userId (SetCardDeck deckName) =
    userId `onlyAdminIsAllowed`
    T ( do
           undefined
      )


-- | Shuffle
-- create an empty game using a given seed
data Shuffle = Shuffle Int
          deriving (Show, Read)
instance ActionC State Shuffle where
  toTransition' userId (Shuffle seed) =
    userId `onlyAdminIsAllowed`
    T ( do
           logForMe ("Shuffle with seed [" ++ show seed ++ "]")
             "Shuffle with seed [only visible for admin]"
           S.state (\state -> (NoWinner, shuffleState seed state))
      )

data DrawDominations = DrawDominations
                     deriving (Show, Read)
instance ActionC State DrawDominations where
  toTransition' userId DrawDominations =
    userId `onlyAdminIsAllowed`
    T ( do
           undefined
      )

-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer String
               deriving (Show, Read)
instance ActionC State AddPlayer where
  toTransition' userId (AddPlayer playerId) =
    userId `onlyAdminIsAllowed`
    T ( do
           log ("Add player: " ++ playerId)
           state <- S.get
           case view machineState state of
             Prepare -> do
               let newPlayer = mkPlayer playerId
               S.state (const (NoWinner, players %~ (newPlayer :) $ state))
             _       -> logError "not in prepare state."
      )

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame
               deriving (Show, Read)
instance ActionC State StartGame where
  toTransition' userId StartGame =
    userId `onlyAdminIsAllowed`
    T ( do
           log "Start game"
           ps <- use players
           if length ps >= 2 && length ps <=4
             then do
             playerOrder .= map getId ps -- FALSE!
             undefined  -- TODO
             else logError "Numer of players is not valid"
      )

data DropPlayer = DropPlayer UserId
                deriving (Show, Read)

data Undo = Undo
          deriving (Show, Read)

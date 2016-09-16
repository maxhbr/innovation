{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Actions.Basic
       ( Draw (..)
       , Play (..)
       , Dominate (..)
       , Activate (..)
       ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import Data.Proxy
import Control.Lens

import Game.MetaGame
import Game.Innovation.Types

--------------------------------------------------------------------------------
-- Chooseable actions
--------------------------------------------------------------------------------
-- | Draw
data Draw = Draw
          deriving (Read, Show)

-- instance UserActionC State Draw where
--   getTransition' _ state | isNothing currentPlayer = fail "not able to Draw"
--                          | otherwise               = W.writer (Right undefined, logs)
--     where
--       currentPlayer  = getPlayerByUserId (getCurrentUser state) state
--       currentDrawAge = getCurrentDrawAge (fromJust currentPlayer) state
--       logs           = undefined

-- | Play
data Play = Play CardId
          deriving (Read, Show)

-- | Dominate
data Dominate = Dominate Age
              deriving (Read, Show)

-- | Activate
data Activate = Activate Color
              deriving (Read, Show)

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Innovation.Actions.Admin
    ( AddPlayer (..)
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
import qualified Control.Lens as L

import           Game.MetaGame
import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import           Game.Innovation.Cards
import           Game.Innovation.Rules
import           Game.Innovation.Actions.Basic

setDeck :: Move Board
setDeck = M $ S.modify (\board -> board{ _drawStacks=getDeck })

drawDominations :: Move Board
drawDominations = M $ do
  ds <- S.gets ((map head) . Map.elems . (L.view L.drawStacks))
  S.modify (\board -> board{ _dominateables=ds })
  S.modify (\board -> board{ _drawStacks=(Map.map drop (_drawStacks board))})
  where
    drop [] = []
    drop cs = tail cs

shuffle :: Int -> Move Board
shuffle seed = M $ do
  logForMe ("Shuffle with seed [" ++ show seed ++ "]")
    "Shuffle with seed [only visible for admin]"
  S.modify (shuffleState seed)

addPlayer :: String -> Move Board
addPlayer playerId = M $ do
  log ("Add player: " ++ playerId)
  mstate <- S.gets _machineState
  case mstate of
    Prepare -> do
      let newPlayer = mkPlayer playerId
      S.modify (L.players L.%~ (newPlayer :))
    _       -> logError "not in prepare state."

--------------------------------------------------------------------------------
-- Admin actions
--------------------------------------------------------------------------------
-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer String
               deriving (Eq, Show, Read)
instance ActionToken Board AddPlayer where
  isAllowedFor (AddPlayer _) Admin = do
    state <- M $ getMachineState
    return $ state == Prepare
  isAllowedFor _ _ = return False

  getAction (AddPlayer newPlayerId) = onlyAdminIsAllowed $
                                      A $ const $ addPlayer newPlayerId

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame Int
               deriving (Eq, Show, Read)
instance ActionToken Board StartGame where
  isAllowedFor (StartGame _) Admin = do
    state <- M $ getMachineState
    return $ state == Prepare
  isAllowedFor _ _ = return False

  getAction (StartGame seed) = onlyAdminIsAllowed $
                               A $ \userId ->
    setDeck <>
    shuffle seed <>
    drawDominations <>
    (M ( do
            log "Start game"
            ps <- L.use L.players
            if length ps >= 2 && length ps <= 4
              then do
              mapM_ (unpackMove . unpackAction (drawNOfAnd 2 Age1) . getUId) ps
              logTODO "ask players for their first card"
              else logError "Numer of players is not valid"
       )) <>
    -- ... wait..
    (M ( do
            -- play chosen cards
            -- determine starting player
            ps <- L.use L.players
            L.playerOrder L..= map getUId ps -- FALSE!
            L.machineState L..= WaitForTurn
       ))

data DropPlayer = DropPlayer UserId
                deriving (Eq, Show, Read)

data Undo = Undo
          deriving (Eq, Show, Read)

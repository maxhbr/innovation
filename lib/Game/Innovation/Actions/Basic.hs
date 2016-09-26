{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Actions.Basic
       ( Draw (..)
       , Play (..)
       , Dominate (..)
       , Activate (..)
       ) where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Data.Proxy
import           Control.Lens

import           Game.MetaGame
import           Game.Innovation.Rules
import           Game.Innovation.Types

data Skip = Skip
          deriving (Eq, Show, Read)
instance ActionToken Board Skip where
  getAction Skip = A $ const $ M $ log "... skip"

-- | Try to draw an card of an specific age
drawAnAnd :: Age -> ActionWR Board Card
drawAnAnd inputAge = A $ \userId -> M $ do
  drawAge <- getDrawAge inputAge
  case drawAge of
    Just age -> do
      card <- S.gets (head . fromJust . (Map.lookup age) . _drawStacks)
      logForMe ("draw the card [" ++ show card ++ "]") ("draw a card of age " ++ show age)
      -- TODO: remove the card from stack
      return card
    _        -> logTODO "endgame..."

-- | Try to draw an card of current age
drawAnd :: ActionWR Board Card
drawAnd = A $ \userId -> M $ do
  actingPlayer <- getPlayerById userId
  let playersAge = getPlayersAge actingPlayer
  unpackMove $ (unpackAction $ drawAnAnd playersAge) userId

drawNAnd :: Int -> ActionWR Board [Card]
drawNAnd n = A $ \userId ->
  M (replicateM n
     (unpackMove
      (unpackAction drawAnd userId)))

putIntoHand :: Card -> Action Board
putIntoHand card = A $ \userId -> M $ do
  actingPlayer <- getPlayerById userId
  let actingPlayer = actingPlayer {_hand = card : (_hand actingPlayer)}
  logTODO "putIntoHand ..."

-- --------------------------------------------------------------------------------
-- -- | Draw an card and put it into an temporary stack
-- data DrawAnd = forall actionToken.
--                (Read actionToken, Show actionToken, ActionToken Board actionToken) =>
--                DrawAnd actionToken

-- instance Eq DrawAnd where
--   (DrawAnd at1) == (DrawAnd at2) = show at1 == show at2

-- instance Read DrawAnd where -- TODO

-- instance Show DrawAnd where
--   show (DrawAnd at) = "DrawAnd " ++ show at

-- instance ActionToken Board DrawAnd where
--   getAction (DrawAnd actionToken) = A $ \userId ->
--     -- Draw
--     -- use actionToken on drawnCard
--     undefined userId

--------------------------------------------------------------------------------
-- | take all cards of the intermediate stack and put them into the hand
data PutIntoHand = PutIntoHand
                 deriving (Eq, Read, Show)
instance ActionToken Board PutIntoHand where
  getAction PutIntoHand = A $ \userId -> M $ do
    logTODO "putIntoHand"

--------------------------------------------------------------------------------
-- | Draw
data Draw = Draw
          deriving (Eq, Read, Show)
instance ActionToken Board Draw where
  getAction Draw = A $ \userId -> M $ do
    unpackMove $ (unpackAction $ drawAnd >>= putIntoHand) userId
-- instance UserActionC State Draw where
--   getTransition' _ state | isNothing currentPlayer = fail "not able to Draw"
--                          | otherwise               = W.writer (Right undefined, logs)
--     where
--       currentPlayer  = getPlayerByUserId (getCurrentUser state) state
--       currentDrawAge = getCurrentDrawAge (fromJust currentPlayer) state
--       logs           = undefined

--------------------------------------------------------------------------------
-- | Play
data Play = Play CardId
          deriving (Eq, Read, Show)
instance ActionToken Board Play where
  getAction (Play cardId) = A $ \userId ->
    undefined

--------------------------------------------------------------------------------
-- | Dominate
data Dominate = Dominate Age
              deriving (Eq, Read, Show)
instance ActionToken Board Dominate where
  getAction (Dominate age) = A $ \userId ->
    undefined

--------------------------------------------------------------------------------
-- | Activate
data Activate = Activate Color
              deriving (Eq, Read, Show)
instance ActionToken Board Activate where
  getAction (Activate color) = A $ \userId ->
    undefined

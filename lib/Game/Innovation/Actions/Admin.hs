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
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Reader (Reader, ReaderT)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import           Game.MetaGame
import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import qualified Game.Innovation.Cards as Cards
import           Game.Innovation.Rules
import           Game.Innovation.Actions.Basic

setDeck :: Move Board
setDeck = M $ S.modify (\board -> board{ _drawStacks=Cards.getDeck })

drawDominations :: Move Board
drawDominations = M $ mapM_ (\age -> do
                                (d,ds) <- S.gets (popCards 1 . fromJust . Map.lookup age . L.view L.drawStacks)
                                S.modify (L.over L.dominateables (pushCards d))
                                S.modify (L.over L.drawStacks (Map.insert age ds))) ages

shuffle :: Int -> Move Board
shuffle seed = M $ do
  logForMe Admin
    ("Shuffle with seed [" ++ show seed ++ "]")
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

determineInitialPlayerOrder :: Move Board
determineInitialPlayerOrder = M $ do
  ps <- L.use L.players
  L.playerOrder L..= map getUId ps -- FALSE! / TODO

handOutInitialCards :: Move Board
handOutInitialCards = do
  M $ do
     ps <- L.use L.players
     log "determin first cards"
     initialCards <- mapM ((\uid -> fmap (\l -> (uid,l))
                                    (uid `takes` drawNOfAnd 2 Age1)) . getUId)
                          ps
     log "ask for first card"
     answeredQuestions <- mapM (\(uid,cs) -> fmap (\[c] -> (uid, List.partition (==c) cs))
                                                  ((uid `chooseOneOf` "the cards, to be played out first") cs))
                               initialCards
     log "all questions for first card were answered, play them"
     mapM_ (\(uid,(playCards,handCards)) -> do
               uid `takes` putIntoPlay playCards
               uid `takes` putIntoHand handCards) answeredQuestions
  determineInitialPlayerOrder
  M $ L.machineState L..= WaitForTurn

--------------------------------------------------------------------------------
-- Admin actions
--------------------------------------------------------------------------------
-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer String
               deriving (Eq, Show, Read)
instance ActionToken Board AddPlayer where
  isAllowedFor (AddPlayer _) Admin = do
    state <- M getMachineState
    return $ state == Prepare
  isAllowedFor _ _ = return False

  getAction (AddPlayer newPlayerId) = (mkAdminA . addPlayer) newPlayerId

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame Int
               deriving (Eq, Show, Read)
instance ActionToken Board StartGame where
  isAllowedFor (StartGame _) Admin = do
    state <- M getMachineState
    return $ state == Prepare
  isAllowedFor _ _ = return False

  getAction (StartGame seed) = mkAdminA $ do
    M $ log "start the game"
    M $ do
      ps <- L.use L.players
      unless (length ps >= 2 && length ps <= 4)
        (logError "Numer of players is not valid")
    setDeck
    shuffle seed
    drawDominations
    handOutInitialCards

-- data DropPlayer = DropPlayer UserId
--                 deriving (Eq, Show, Read)

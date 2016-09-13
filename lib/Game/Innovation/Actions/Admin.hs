{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Innovation.Actions.Admin
    ( Init (..)
    , AddPlayer (..)
    , StartGame (..))
    where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import System.Random
import System.Random.Shuffle (shuffle')

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Cards
import Game.Innovation.Rules

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

mkPlayer :: UserId -> Player
mkPlayer playerId = Player playerId
                           (Map.fromList $ zip colors $ repeat [])
                           (Map.fromList $ zip colors $ repeat NotSplayed)
                           []
                           []
                           []

mkInitialState :: Int -> State
mkInitialState seed = State permutatedDrawStack
                            []
                            []
                            []
  where
    stdGen = mkStdGen seed
    shuffle []    = []
    shuffle stack = shuffle' stack (length stack) stdGen
    permutatedDrawStack = Map.map shuffle cards

--------------------------------------------------------------------------------
-- Admin actions
--------------------------------------------------------------------------------

-- | Init
-- create an empty game using a given seed
data Init = Init Int
          deriving (Show, Read)
instance UserActionC State Init where
  getTransition' (Init seed) Q0 = W.writer ( Right $ Prepare $ mkInitialState seed
                                           , [T.pack . \case
                                                 Nothing -> "Init [with seed " ++ show seed ++ "]"
                                                 Just _  -> "Init [with seed only visible for admin]"])
  getTransition' _ _            = fail "Game was already inited"
  isMetaAction' = const True

-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer UserId
               deriving (Show, Read)
instance UserActionC State AddPlayer where
  getTransition' (AddPlayer playerId) (Prepare state) = W.writer ( Right $
                                                                   Prepare $
                                                                   state { getPlayers = mkPlayer playerId : getPlayers state
                                                                         , getPlayerOrder = playerId : getPlayerOrder state }
                                                                 , [T.pack . const ("AddUser " ++ playerId)])
  getTransition' _ _                                  = fail "Game was not in prepare state"
  isMetaAction' = const True

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame
               deriving (Show, Read)
instance UserActionC State StartGame where
  getTransition' _ (Prepare state) = W.writer ( Right state
                                              , [T.pack . const "StartGame"])
  getTransition' _ _               = fail "Game was not in prepare state"
  isMetaAction' = const True

-- data DropPlayer = DropPlayer UserId
--                 deriving (Show, Read)

-- data Undo = Undo
--           deriving (Show, Read)

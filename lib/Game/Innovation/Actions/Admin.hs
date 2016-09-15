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
import Control.Lens

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Cards
import Game.Innovation.Rules

--------------------------------------------------------------------------------
-- Admin actions
--------------------------------------------------------------------------------

-- | Init
-- create an empty game using a given seed
data Init = Init Int
          deriving (Show, Read)
-- instance ActionC State Init where
--   toTransition' Admin (Init seed) Q0 = W.writer ( Right $ Prepare $ mkInitialState cards seed
--                                                 , [T.pack . \case
--                                                      Admin -> "Init [with seed " ++ show seed ++ "]"
--                                                      U _  -> "Init [with seed only visible for admin]"])
--   toTransition' _ _ _                  = fail "Game was already inited or action was not authorized"
--   isMetaAction' = const True

-- | AddPlayer
-- add an player with a given playerId to the game
data AddPlayer = AddPlayer String
               deriving (Show, Read)
-- instance ActionC State AddPlayer where
--   toTransition' Admin (AddPlayer playerId) (Prepare state) = W.writer ( Right $
--                                                                         Prepare $
--                                                                         state { _players     = mkPlayer playerId : view players state
--                                                                               , _playerOrder = U playerId : view playerOrder state }
--                                                                       , [T.pack . const ("AddUser " ++ playerId)])
--   -- allow user to add itself to a game?
--   toTransition' _ _ _                                        = fail "Game was not in prepare state or action was not authorized"
--   isMetaAction' = const True

-- | StartGame
-- finish preperations of the game
data StartGame = StartGame
               deriving (Show, Read)
-- instance ActionC State StartGame where
--   toTransition' Admin _ (Prepare state) = W.writer ( Right state
--                                                    , [T.pack . const "StartGame"])
--   toTransition' _ _ _                   = fail "Game was not in prepare state or action was not authorized"
--   isMetaAction' = const True

data DropPlayer = DropPlayer UserId
                deriving (Show, Read)

data Undo = Undo
          deriving (Show, Read)

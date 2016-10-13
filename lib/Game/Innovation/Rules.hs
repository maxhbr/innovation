{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Rules
    where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Data.Proxy
import qualified Control.Lens as L

import           Game.MetaGame
import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import           Game.Innovation.CoreRules

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getStackFromMapBy :: (Ord k, Stack s) =>
                     k -> Map k s -> s
getStackFromMapBy = Map.findWithDefault emptyStack

getPlayerById :: UserId -> MoveType Board Player
getPlayerById uid = do
  players <- S.gets _players
  let playersWithId = filter (\p -> getUId p == uid) players
  case playersWithId of
    [p] -> return p
    []  -> logError "player not found"
    _   -> logError "multiple players found, with the same id"

--------------------------------------------------------------------------------
-- Getter for ages
--------------------------------------------------------------------------------

getPlayersAge :: Player -> Age
getPlayersAge player = let
  currentAges = (map (_age . head . getRawStack) . filter (not . isEmptyStack) . Map.elems) $ _playStacks player
  in if currentAges /= []
     then maximum currentAges
     else Age1

getDrawAge :: Age -> MoveType Board (Maybe Age)
getDrawAge inputAge = do
  currentDrawStacks <- S.gets _drawStacks
  let agesAboveWithCards = Map.keys $
                           Map.filterWithKey (\ age stack -> age >= inputAge
                                                             && (not . isEmptyStack) stack) currentDrawStacks
  return $ if null agesAboveWithCards
           then Nothing
           else Just $ head agesAboveWithCards

getPlayersDrawAge :: Player -> MoveType Board (Maybe Age)
getPlayersDrawAge player = do
  let playersAge = getPlayersAge player
  getDrawAge playersAge

--------------------------------------------------------------------------------
-- Getter for visible productions and related symbols
--------------------------------------------------------------------------------

getSymbols :: Player -> Map Symbol Int
getSymbols player = Map.fromListWith (+) $
                    zip playersSymbolsList (repeat 1)
  where
    playersSymbolsList = map _prodSymbol $
                         filter isSymbolProduction $
                         getProductions player

getProductions :: Player -> [Production]
getProductions player = concatMap (`getProductionsForColor` player) colors

getProductionsForColor :: Color -> Player -> [Production]
getProductionsForColor color player = getProductionsForStack stackOfColor
  where
    stackOfColor      = getStackFromMapBy color $ L.view L.playStacks player

getProductionsForStack :: PlayStack -> [Production]
getProductionsForStack (PlayStack [] _)              = []
getProductionsForStack (PlayStack [card] _)          = map ((\v -> v (_productions card)) . L.view)
                                                       [ L.tlProd, L.blProd, L.bcProd, L.brProd ]
getProductionsForStack (PlayStack (c:cs) splayState) = getProductionsForStack (PlayStack [c] splayState) ++ prodOfInactive
  where
    prodOfInactive = concatMap (getVisible splayState) cs

    getVisible :: SplayState -> Card -> [Production]
    getVisible s c = map (\l -> (L.view l . _productions) c) (getLenses s)

    getLenses SplayedLeft  = [ L.brProd ]
    getLenses SplayedRight = [ L.tlProd, L.blProd ]
    getLenses SplayedUp    = [ L.blProd, L.bcProd, L.brProd ]
    getLenses NotSplayed   = []


getInfluence :: Player -> Int
getInfluence (Player _ _ (Influence is) _ _) = sum (map (fromEnum . _age) is)

--------------------------------------------------------------------------------
-- Getter for visible productions and related symbols
--------------------------------------------------------------------------------

endGame :: MoveWR Board a
endGame = M $ do
  log "endgame"
  ps <- L.use L.players
  let influences = map (_playerId &&& getInfluence) ps
  let maxInfluence = maximum (map snd influences)
  log $ "greatest influnce is: " ++ show maxInfluence
  let winner = head [uid | (uid,infl) <- influences
                         , infl == maxInfluence] -- TODO
  S.modify (setMachineState' (GameOver winner))
  S.get >>= (lift . lift . E.throwE)


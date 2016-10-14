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
    _   -> logFatal "multiple players found, with the same id"

getUidsWith :: (Player -> Bool) -> MoveType Board [UserId]
getUidsWith t = do
  ps <- fmap (filter t) (L.use L.players)
  return (map _playerId ps)


--------------------------------------------------------------------------------
-- Getter for ages
--------------------------------------------------------------------------------

ageOf :: Player -> Age
ageOf player = let
  currentAges = (map (_age . head . getRawStack) . filter (not . isEmptyStack) . Map.elems) $ _playStacks player
  in if currentAges /= []
     then maximum currentAges
     else Age1

getAgeOf :: UserId -> MoveType Board Age
getAgeOf = fmap ageOf . getPlayerById

getDrawAge :: Age -> MoveType Board (Maybe Age)
getDrawAge inputAge = do
  currentDrawStacks <- S.gets _drawStacks
  let agesAboveWithCards = Map.keys $
                           Map.filterWithKey (\ age stack -> age >= inputAge
                                                             && (not . isEmptyStack) stack) currentDrawStacks
  return $ if null agesAboveWithCards
           then Nothing
           else Just $ head agesAboveWithCards

getDrawAgeOf :: UserId -> MoveType Board (Maybe Age)
getDrawAgeOf uid = getAgeOf uid >>= getDrawAge

--------------------------------------------------------------------------------
-- Getter for visible productions and related symbols
--------------------------------------------------------------------------------

listToFrequency :: Ord a =>
                   [a] -> Map a Int
listToFrequency list = Map.fromListWith (+) (zip list (repeat 1))

productionsForStack :: PlayStack -> Map Symbol Int
productionsForStack = listToFrequency . map _prodSymbol . filter isSymbolProduction . productionsForStack'

productionsForStack' :: PlayStack -> [Production]
productionsForStack' (PlayStack [] _)              = []
productionsForStack' (PlayStack [card] _)          = map ((\v -> v (_productions card)) . L.view)
                                                     [ L.tlProd, L.blProd, L.bcProd, L.brProd ]
productionsForStack' (PlayStack (c:cs) splayState) = productionsForStack' (PlayStack [c] splayState) ++ prodOfInactive
  where
    prodOfInactive = concatMap (getVisible splayState) cs

    getVisible :: SplayState -> Card -> [Production]
    getVisible s c = map (\l -> (L.view l . _productions) c) (getLenses s)

    getLenses SplayedLeft  = [ L.brProd ]
    getLenses SplayedRight = [ L.tlProd, L.blProd ]
    getLenses SplayedUp    = [ L.blProd, L.bcProd, L.brProd ]
    getLenses NotSplayed   = []

productionsForColorOf :: Color -> Player -> Map Symbol Int
productionsForColorOf color player = let
  stackOfColor = getStackFromMapBy color (L.view L.playStacks player)
  in productionsForStack stackOfColor

productionsOf :: Player -> Map Symbol Int
productionsOf player = Map.unionsWith (+) (map (`productionsForColorOf` player) colors)

getProductionsOf :: UserId -> MoveType Board (Map Symbol Int)
getProductionsOf uid = do
  player <- getPlayerById uid
  return (productionsOf player)

getProductionsForSymbolOf :: UserId -> Symbol -> MoveType Board Int
getProductionsForSymbolOf uid symb = fmap (Map.findWithDefault 0 symb) (getProductionsOf uid)

--------------------------------------------------------------------------------

doEndGame :: MoveWR Board a
doEndGame = M $ do
  log "endgame"
  ps <- L.use L.players
  let influences = map (_playerId &&& getInfluence) ps
  let maxInfluence = maximum (map snd influences)
  log $ "greatest influnce is: " ++ show maxInfluence
  let winner = head [uid | (uid,infl) <- influences
                         , infl == maxInfluence] -- TODO
  S.modify (setMachineState' (GameOver winner))
  S.get >>= (lift . lift . E.throwE)

--------------------------------------------------------------------------------

runDogma :: Dogma -> Action Board
runDogma dogma = let
  symb = getDSymbol dogma
  -- determineAffected =
  -- determineAffected = undefined
  comperator callersNum = case dogma of
    Dogma{}   -> (>= callersNum)
    IDemand{} -> (< callersNum)
  in mkA $ \uid -> do
    callersNum <- getProductionsForSymbolOf uid symb
    affected <- undefined -- TODO
    mapM_ undefined affected -- TODO

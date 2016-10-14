{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.Innovation.Rules.CoreRules
       where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Game.MetaGame
import           Game.Innovation.Types

isPlayerWinnerByDomination :: Int -- ^ number of players
                           -> Player -- ^ player of interest
                           -> Bool -- ^ obvious
isPlayerWinnerByDomination n Player{ _dominations=(Dominations ds) } = let
  numOfDs = length ds
  in case n of
  2 -> numOfDs > 6
  3 -> numOfDs > 5
  4 -> numOfDs > 4
  _ -> False

-- determineWinnerByInfluence  -- TODO

instance BoardC Board where
  emptyBoard = Board Prepare Map.empty
                     emptyStack
                     []
                     []
                     []

  getMachineState' = _machineState

  setMachineState' s b = b{ _machineState=s }

  getCurrentPlayer' Board{ _machineState=(GameOver _) }     = Admin
  getCurrentPlayer' Board{ _machineState=WaitForChoice cs } = askedPlayer cs
  getCurrentPlayer' Board{ _machineState=Prepare }          = Admin
  getCurrentPlayer' Board{ _playerOrder=[] }                = Admin
  getCurrentPlayer' Board{ _playerOrder=order }             = head order

  advancePlayerOrder b@Board{ _playerOrder = ps } = b{ _playerOrder = (advancePlayerOrder' ps) }
    where
      advancePlayerOrder' :: PlayerOrder -> PlayerOrder
      advancePlayerOrder' []                       = []
      advancePlayerOrder' [p]                      = [p]
      advancePlayerOrder' (p1:(p2:ps)) | p1 == p2  = p2:ps -- ^ one consumes actions as long as it is not the last one
                                       | otherwise = p2:ps ++ [p1,p1] -- ^ every player gets two actions in his next round

  doAtomicUpdate b0 = do
    b1 <- doSpecialAchievements b0
    b2 <- determineWinner b1
    return b2
    where
      doSpecialAchievements = return . id -- TODO
      determineWinner = return . id -- TODO

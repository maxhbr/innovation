{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Game.Innovation.Rules.CoreRules
       where
import           Prelude hiding (log)
import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import qualified Game.MetaGame as MG
import           Game.Innovation.Types

--------------------------------------------------------------------------------

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

determineWinnerByInfluence :: [Player] -> Either String UserId
determineWinnerByInfluence ps = let
  influences = map (_playerId &&& getInfluence) ps
  maxInfluence = maximum (map snd influences)
  winners = [uid | (uid,infl) <- influences
                 , infl == maxInfluence]
  in case winners of
    [w] -> Right w
    []  -> Left "no one has influence equal to max influence?"
    ws   -> Left ("multiple winners by inluence: " ++ show ws)

--------------------------------------------------------------------------------

doEndGame :: MoveWR a
doEndGame = M $ do
  log "endgame"
  ps <- getPlayers
  let influences = map (_playerId &&& getInfluence) ps
  let maxInfluence = maximum (map snd influences)
  log $ "greatest influnce is: " ++ show maxInfluence
  let winner = head [uid | (uid,infl) <- influences
                         , infl == maxInfluence] -- TODO
  setMachineState (GameOver winner)
  S.get >>= (lift . lift . E.throwE)

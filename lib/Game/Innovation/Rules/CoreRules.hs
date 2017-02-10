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

getPlayers :: MoveType [UserId]
getPlayers = MG.getIdFyObject "getPlayers"

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
  psIds <- getPlayers
  ps <- mapM getObject psIds
  let influences = map (_playerId &&& getInfluence) ps
  let maxInfluence = maximum (map snd influences)
  log $ "greatest influnce is: " ++ show maxInfluence
  let winner = head [uid | (uid,infl) <- influences
                         , infl == maxInfluence] -- TODO
  setMachineState (GameOver winner)
  S.get >>= (lift . lift . E.throwE)

--------------------------------------------------------------------------------
data BoardId = BoardId
             deriving (Eq,Show)
type instance IdF Board = BoardId
instance IdAble Board where
  idOf _ = BoardId

-- instance BoardC Board where
--   emptyBoard = Board Prepare Map.empty
--                      emptyStack
--                      []
--                      []
--                      []

--   getMachineState' = _machineState

--   setMachineState' s b = b{ _machineState=s }

--   getCurrentPlayer' Board{ _machineState=(GameOver _) }     = Admin
--   getCurrentPlayer' Board{ _machineState=WaitForChoice cs } = askedPlayer cs
--   getCurrentPlayer' Board{ _machineState=Prepare }          = Admin
--   getCurrentPlayer' Board{ _playerOrder=[] }                = Admin
--   getCurrentPlayer' Board{ _playerOrder=order }             = head order

--   advancePlayerOrder b@Board{ _playerOrder = ps } = b{ _playerOrder = (advancePlayerOrder' ps) }
--     where
--       advancePlayerOrder' :: PlayerOrder -> PlayerOrder
--       advancePlayerOrder' []                       = []
--       advancePlayerOrder' [p]                      = [p]
--       advancePlayerOrder' (p1:(p2:ps)) | p1 == p2  = p2:ps -- ^ one consumes actions as long as it is not the last one
--                                        | otherwise = p2:ps ++ [p1,p1] -- ^ every player gets two actions in his next round

--   doAtomicUpdate b0 = do
--     b1 <- doSpecialAchievements b0
--     b2 <- determineWinner b1
--     return b2
--     where
--       doSpecialAchievements = return . id -- TODO
--       determineWinner = return . id -- TODO

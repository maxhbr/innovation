module Game.Innovation.Types.InnovationState
       ( idForUIds, idForDrawStacks, idForDominateables
       , getUIds, setUIds
       , getPlayer, setPlayer, addPlayerM, modifyPlayer
       , getPlayers
       , getPlayersWith, getUIdsWith
       , getDrawStacks, setDrawStacks, getDrawStackOf, setDrawStackOf, drawOfStackOf
       , getDominateables, setDominateables, modifyDominateables, removeFromDominateables
       ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import           Control.Monad

import           Game.MetaGame ( getObject, setObject, modifyObject
                               , getIdFyObject, setIdFyObject, modifyIdFyObject )
import           Game.Innovation.Types.Core
import           Game.Innovation.Types.StackHelper

idForUIds, idForDrawStacks, idForDominateables :: String
idForUIds = "UIds"
idForDrawStacks = "DrawStacks"
idForDominateables = "idForDominateables"

-- | returns list of playing UserIds **in order**
getUIds :: MoveType [UserId]
getUIds = getIdFyObject idForUIds
setUIds :: [UserId] -> MoveType ()
setUIds = setIdFyObject idForUIds
modifyUIds :: ([UserId] -> [UserId]) -> MoveType ()
modifyUIds f = modifyIdFyObject f idForUIds

getPlayer :: UserId -> MoveType Player
getPlayer = getObject
setPlayer :: Player -> MoveType ()
setPlayer player = do
  uids <- getUIds
  unless ((idOf player) `List.elem` uids) $
    logFatal ("Player with ID=" ++ show (idOf player) ++ " not in list of IDs")
  setObject player
addPlayerM :: Player -> MoveType ()
addPlayerM player = do
  setPlayer player
  modifyUIds (idOf player :)
modifyPlayer :: (Player -> Player) -> UserId -> MoveType ()
modifyPlayer = modifyObject

getPlayers :: MoveType [Player]
getPlayers = getUIds >>= mapM getPlayer

getPlayersWith :: (Player -> Bool) -> MoveType [Player]
getPlayersWith f = fmap (List.filter f) getPlayers
getUIdsWith :: (Player -> Bool) -> MoveType [UserId]
getUIdsWith f = fmap ((List.map idOf) . List.filter f) getPlayers

getDrawStacks :: MoveType (Map Age DrawStack)
getDrawStacks = getIdFyObject idForDrawStacks
setDrawStacks :: Map Age DrawStack -> MoveType ()
setDrawStacks = setIdFyObject idForDrawStacks
getDrawStackOf :: Age -> MoveType DrawStack
getDrawStackOf age = do
  maybeStack <- fmap (Map.lookup age) getDrawStacks
  case maybeStack of
    Just stack -> return stack
    Nothing    -> logError ("Tried to find stack of age: " ++ show age)
setDrawStackOf :: Age -> DrawStack -> MoveType ()
setDrawStackOf age stack = modifyIdFyObject (Map.insert age stack) idForDrawStacks

drawOfStackOf :: Age -> MoveType (Maybe Card)
drawOfStackOf age = do
  stack <- getDrawStackOf age
  let (cards, rest) = popCard stack
  setDrawStackOf age rest
  case cards of
     [card] -> return (Just card)
     []     -> return Nothing
     _      -> logFatal "should not be reacheable"

getDominateables :: MoveType Dominateables
getDominateables = getIdFyObject idForDominateables
setDominateables :: Dominateables -> MoveType ()
setDominateables = setIdFyObject idForDominateables
modifyDominateables :: (Dominateables -> Dominateables) -> MoveType ()
modifyDominateables f = modifyIdFyObject f idForDominateables
removeFromDominateables :: Card -> MoveType ()
removeFromDominateables c = do
  ds <- getDominateables
  if (c `List.elem` (getRawStack ds))
    then setDominateables (removeFromStack c ds)
    else logError ("Tried to remove nonexisting dominateable: " ++ show c)

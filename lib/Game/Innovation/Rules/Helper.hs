module Game.Innovation.Rules.Helper
    where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import           Game.Innovation.Rules.CoreRules ()

liftToGet :: (Player -> a) -> UserId -> MoveType a
liftToGet f uid = do
  player <- getPlayerById uid
  return (f player)

lift2ToGet :: (b -> Player -> a) -> b -> UserId -> MoveType a
lift2ToGet f b uid = do
  player <- getPlayerById uid
  return (f b player)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

stackFromMapBy :: (Ord k, Stack s) =>
                     k -> Map k s -> s
stackFromMapBy = Map.findWithDefault emptyStack

getPlayerById :: UserId -> MoveType Player
getPlayerById uid = do
  players <- S.gets _players
  let playersWithId = filter (\p -> idOf p == uid) players
  case playersWithId of
    [p] -> return p
    []  -> logError "player not found"
    _   -> logFatal "multiple players found, with the same id"

getUidsWith :: (Player -> Bool) -> MoveType [UserId]
getUidsWith t = do
  ps <- fmap (filter t) (L.use L.players)
  return (map _playerId ps)

--------------------------------------------------------------------------------
-- Getter for ages
--------------------------------------------------------------------------------

getActiveCards :: Player -> RawStack
getActiveCards player = (map (head . getRawStack) . filter (not . isEmptyStack) . Map.elems) (_zone player)

ageOf :: Player -> Age
ageOf player = let
  currentAges = map _age (getActiveCards player)
  in if currentAges /= []
     then maximum currentAges
     else Age1

getAgeOf :: UserId -> MoveType Age
getAgeOf = fmap ageOf . getPlayerById

getDrawAgeByAge :: Age -> MoveType (Maybe Age)
getDrawAgeByAge inputAge = do
  currentDrawStacks <- S.gets _drawStacks
  let agesAboveWithCards = Map.keys $
                           Map.filterWithKey (\ age stack -> age >= inputAge
                                                             && (not . isEmptyStack) stack)
                                             currentDrawStacks
  return $ if null agesAboveWithCards
           then Nothing
           else Just $ head agesAboveWithCards

getDrawAgeOf :: UserId -> MoveType (Maybe Age)
getDrawAgeOf uid = getAgeOf uid >>= getDrawAgeByAge

getDrawAge :: ActionType (Maybe Age)
getDrawAge = R.ask >>= (lift . getDrawAgeOf)

--------------------------------------------------------------------------------
-- Getter for visible productions and related symbols
--------------------------------------------------------------------------------

getHandOf :: UserId -> MoveType Hand
getHandOf = liftToGet _hand

playStackByColorOf :: Color -> Player -> PlayStack
playStackByColorOf col player = stackFromMapBy col (L.view L.zone player)

getPlayStackByColorOf :: Color -> UserId -> MoveType PlayStack
getPlayStackByColorOf = lift2ToGet playStackByColorOf

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
    getVisible state card = map (\l -> (L.view l . _productions) card) (getLenses state)

    getLenses SplayedLeft  = [ L.brProd ]
    getLenses SplayedRight = [ L.tlProd, L.blProd ]
    getLenses SplayedUp    = [ L.blProd, L.bcProd, L.brProd ]
    getLenses NotSplayed   = []

productionsForColorOf :: Color -> Player -> Map Symbol Int
productionsForColorOf col player = productionsForStack (playStackByColorOf col player)

productionsForSymbolOf :: Symbol -> Player -> Int
productionsForSymbolOf symb = Map.findWithDefault 0 symb . productionsOf

productionsOf :: Player -> Map Symbol Int
productionsOf player = Map.unionsWith (+) (map (`productionsForColorOf` player) colors)

getProductionsOf :: UserId -> MoveType (Map Symbol Int)
getProductionsOf = liftToGet productionsOf

getProductionsForSymbolOf :: Symbol -> UserId -> MoveType Int
getProductionsForSymbolOf symb uid = fmap (Map.findWithDefault 0 symb) (getProductionsOf uid)

modifyPlayer :: UserId -> (Player -> Player) -> MoveType ()
modifyPlayer userId f = do
  playerToModify <- getPlayerById userId
  let modifiedPlayer = f playerToModify
  S.modify $ \b -> b {_players = modifiedPlayer : filter (\p -> not $ p `hasId` userId) (_players b)}

playedColorsOf :: Player -> [Color]
playedColorsOf Player{ _zone=ps } = [c | c <- colors
                                       , (not . isEmptyStack) (Map.findWithDefault emptyStack c ps)]

getPlayedColorsOf :: UserId -> MoveType [Color]
getPlayedColorsOf = liftToGet playedColorsOf

influenceOf :: Player -> Int
influenceOf = sum . map ((+1) . fromEnum . _age) . getRawStack . _influence

getInfluenceOf :: UserId -> MoveType Int
getInfluenceOf = liftToGet influenceOf

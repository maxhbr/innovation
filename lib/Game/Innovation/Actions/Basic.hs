{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Actions.Basic
       -- ( Skip (..), skip
       -- , Draw (..)
       -- , Play (..)
       -- , Dominate (..)
       -- , Activate (..)
       -- )
       where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.List as List
import           Control.Monad
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
import           Game.Innovation.Rules
import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L


takeCard :: Stack -> ([Card], Stack)
takeCard = takeN 1

takeTheCard :: CardId -> Stack -> (Maybe Card, Stack)
takeTheCard cid cs = let
  (c1,c2) = List.partition (\c -> getCId c == cid) cs
  in case c1 of
    [c] -> (Just c, c2)
    []  -> (Nothing, c2)
    _   -> undefined -- TODO: should not be reacheable

takeN :: Int -> Stack -> ([Card], Stack)
takeN i s = takeN' i ([],s)
  where
    takeN' 0 r            = r
    takeN' i (cs, [])     = (cs,[])
    takeN' i (cs, (s:ss)) = takeN' (i-1) (s:cs, ss)

putAtTop :: [Card] -> Stack -> Stack
putAtTop cs = (cs ++)

putAtBottom :: [Card] -> Stack -> Stack
putAtBottom cs = (++ cs)

--------------------------------------------------------------------------------
-- * Raw actions

--------------------------------------------------------------------------------
-- | do nothing
skip :: Action Board
skip = mkA . const $ log "... skip"
data Skip = Skip
          deriving (Eq, Show, Read)
instance ActionToken Board Skip where
  getAction Skip = skip

modifyPlayer :: UserId -> (Player -> Player) -> MoveType Board ()
modifyPlayer userId f = do
  playerToModify <- getPlayerById userId
  let modifiedPlayer = f playerToModify
  S.modify $ \b -> b {_players = modifiedPlayer : (filter (\p -> not $ p `hasUId` userId) (_players b))}

drawNOfAnd :: Int -> Age -> ActionWR Board [Card]
drawNOfAnd n age = mkA $ \userId ->  fmap concat (replicateM n
                                                  (unpackMove
                                                   (unpackAction (drawOfAnd age) userId)))

-- | Try to draw an card of an specific age
drawOfAnd :: Age -> ActionWR Board [Card]
drawOfAnd inputAge = mkA $ \userId -> do
  drawAge <- getDrawAge inputAge
  case drawAge of
    Just age -> do
      stack <- S.gets (fromJust . (Map.lookup age) . _drawStacks)
      let (cards, rest) = takeCard stack
      S.modify $ L.over L.drawStacks (Map.insert age rest)
      case cards of
        [card] -> do
          logForMe ("draw the card " ++ pp card) ("draw a card of age " ++ pp age)
          return [card]
        []     -> logTODO "endgame..."
        _      -> logFatal "should not be reacheable"
    _        -> logTODO "endgame..."

-- | Try to draw an card of current age
drawAnd :: ActionWR Board [Card]
drawAnd = mkA $ \userId -> do
  actingPlayer <- getPlayerById userId
  let playersAge = getPlayersAge actingPlayer
  userId `takes` drawOfAnd playersAge

drawNAnd :: Int -> ActionWR Board [Card]
drawNAnd n = mkA $ \userId -> fmap concat (replicateM n
                                           (unpackMove
                                            (unpackAction drawAnd userId)))

putIntoHand :: [Card] -> Action Board
putIntoHand cards = mkA $ \userId ->
  modifyPlayer userId $ L.over L.hand (cards ++)

putIntoPlay :: [Card] -> Action Board
putIntoPlay card = mkA $ \userId -> let
  put1IntoPlay :: Card -> MoveType Board ()
  put1IntoPlay card = do
    log ("put the card " ++ pp card ++ " into play")
    let color = L.view L.color card
    modifyPlayer userId $ L.over L.stacks (Map.adjust (card :) color)
  in mapM_ put1IntoPlay card

score :: [Card] -> Action Board
score cards = mkA $ \userId ->
  modifyPlayer userId $ L.over L.influence (cards ++)

dominate :: Age -> Stack -> (Maybe Card, Stack)
dominate age = dominate' []
  where
    dominate' scanned []                     = (Nothing, scanned)
    dominate' scanned (c:cs) | _age c == age = (Just c, scanned ++ cs)
                             | otherwise     = dominate' (c:scanned) cs

-- --------------------------------------------------------------------------------
-- -- | Draw an card and put it into an temporary stack
-- data DrawAnd = forall actionToken.
--                (Read actionToken, Show actionToken, ActionToken Board actionToken) =>
--                DrawAnd actionToken

-- instance Eq DrawAnd where
--   (DrawAnd at1) == (DrawAnd at2) = show at1 == show at2

-- instance Read DrawAnd where -- TODO

-- instance Show DrawAnd where
--   show (DrawAnd at) = "DrawAnd " ++ show at

-- instance ActionToken Board DrawAnd where
--   getAction (DrawAnd actionToken) = A $ \userId ->
--     -- Draw
--     -- use actionToken on drawnCard
--     undefined userId

--------------------------------------------------------------------------------
-- -- | take all cards of the intermediate stack and put them into the hand
-- data PutIntoHand = PutIntoHand
--                  deriving (Eq, Read, Show)
-- instance ActionToken Board PutIntoHand where
--   getAction PutIntoHand = A $ \userId -> M $ do
--     logTODO "putIntoHand"

--------------------------------------------------------------------------------
-- | Draw
data Draw = Draw
          deriving (Eq, Read, Show)
instance ActionToken Board Draw where
  getAction Draw = drawAnd >>= putIntoHand

--------------------------------------------------------------------------------
-- | Play
data Play = Play CardId
          deriving (Eq, Read, Show)
instance ActionToken Board Play where
  getAction (Play cardId) = mkA $ \userId -> do
    player <- getPlayerById userId
    let (cardM, rest) = takeTheCard cardId $ L.view L.hand player
    case cardM of
      Just card -> do
        modifyPlayer userId $ \p -> p{ _hand=rest }
        userId `takes` putIntoPlay [card]
      Nothing   -> logError "card not in the hand"

--------------------------------------------------------------------------------
-- | Dominate
data Dominate = Dominate Age
              deriving (Eq, Read, Show)
instance ActionToken Board Dominate where
  getAction (Dominate age) = mkA $ \userId ->
    undefined

--------------------------------------------------------------------------------
-- | Activate
data Activate = Activate Color
              deriving (Eq, Read, Show)
instance ActionToken Board Activate where
  getAction (Activate color) = mkA $ \userId ->
    undefined

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


pushCards :: Stack a =>
             [Card] -> a -> a
pushCards cs = onRawStack (cs ++)

pushBottomCards :: Stack a =>
                   [Card] -> a -> a
pushBottomCards cs = onRawStack (++ cs)

popCards :: Stack a =>
            Int -> a -> ([Card], a)
popCards n a = ((\(cs, rs) -> (cs, setRawStack a rs)) . popCards' n . getRawStack) a
  where
    popCards' :: Int -> RawStack -> ([Card], RawStack)
    popCards' 0 rs     = ([], rs)
    popCards' _ []     = ([], [])
    popCards' n (r:rs) = (\(cs',rs') -> (r: cs', rs')) (popCards' (n-1) rs)

popCardsWith :: Stack a =>
                Int -> (Card -> Bool) -> a -> ([Card],a)
popCardsWith n p a = (\(res,rs) -> (res, setRawStack a rs)) (popCardsWith' n p (getRawStack a))
  where
    popCardsWith' 0 _ a      = ([], a)
    popCardsWith' _ _ []     = ([], [])
    popCardsWith' n b (r:rs) | b r       = (\(cs',rs') -> (r: cs', rs')) (popCardsWith' (n-1) b rs)
                             | otherwise = (\(cs',rs') -> (cs', r: rs')) (popCardsWith' (n-1) b rs)

popTheCard :: Stack a =>
              CardId -> a -> (Maybe Card, a)
popTheCard cid a = let
  (rs1,rs2) = List.partition (\c -> getCId c == cid) (getRawStack a) -- TODO: respects order??
  in (\(res,rs) -> (res, setRawStack a rs)) (case rs1 of
                                                [c] -> (Just c, rs2)
                                                []  -> (Nothing, rs2)
                                                _   -> undefined -- TODO: should not be reacheable
                                            )

popCard :: Stack a =>
           a -> ([Card], a)
popCard = popCards 1

pushCard :: Stack a =>
            Card -> a -> a
pushCard c = pushCards [c]

-- takeTheCard :: CardId -> Stack -> (Maybe Card, Stack)
-- takeTheCard cid cs = let
--   (c1,c2) = List.partition (\c -> getCId c == cid) cs
--   in case c1 of
--     [c] -> (Just c, c2)
--     []  -> (Nothing, c2)
--     _   -> undefined -- TODO: should not be reacheable

-- takeN :: Int -> Stack -> ([Card], Stack)
-- takeN i s = takeN' i ([],s)
--   where
--     takeN' 0 r            = r
--     takeN' i (cs, [])     = (cs,[])
--     takeN' i (cs, (s:ss)) = takeN' (i-1) (s:cs, ss)

-- putAtTop :: [Card] -> Stack -> Stack
-- putAtTop cs = (cs ++)

-- putAtBottom :: [Card] -> Stack -> Stack
-- putAtBottom cs = (++ cs)

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
      let (cards, rest) = popCard stack
      S.modify $ L.over L.drawStacks (Map.insert age rest)
      case cards of
        [card] -> do
          logForMe ("draw the card " ++ show card) ("draw a card of age " ++ show age)
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
  modifyPlayer userId $ L.over L.hand (onRawStack (cards ++))

putIntoPlay :: [Card] -> Action Board
putIntoPlay card = mkA $ \userId -> let
  put1IntoPlay :: Card -> MoveType Board ()
  put1IntoPlay card = do
    log ("put the card " ++ show card ++ " into play")
    let color = _color card
    modifyPlayer userId $ L.over L.playStacks (Map.adjust (pushCard card) color)
  in mapM_ put1IntoPlay card

score :: [Card] -> Action Board
score cards = mkA $ \userId ->
  modifyPlayer userId $ L.over L.influence (pushCards cards)

dominateAge :: Age -> Action Board
dominateAge age = mkA $ \userId -> undefined
  -- let
  -- dominateAge' :: Stack -> Stack -> (Maybe Card, Stack)
  -- dominateAge' scanned []                     = (Nothing, scanned)
  -- dominateAge' scanned (c:cs) | _age c == age = (Just c, scanned ++ cs)
  --                             | otherwise     = dominateAge' (c:scanned) cs
  -- in do
  -- (mc, ds) <- S.gets ((dominateAge' []) . (L.view L.dominateables))
  -- case mc of
  --   Just card -> do
  --     log ("dominate age " ++ show age)

  --     S.modify $ \b -> b { _dominateables=ds }
  --     modifyPlayer userId $ L.over L.dominations (card :)
  --   Nothing   -> logError $ "there is no card of age " ++ show age ++ " to be dominated"

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
    let (cardM, rest) = popTheCard cardId $ L.view L.hand player
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
  getAction (Dominate age) = mkA $ \userId -> do
    logTODO "check prerequisits"
    userId `takes` dominateAge age

--------------------------------------------------------------------------------
-- | Activate
data Activate = Activate Color
              deriving (Eq, Read, Show)
instance ActionToken Board Activate where
  getAction (Activate color) = mkA $ \userId ->
    undefined

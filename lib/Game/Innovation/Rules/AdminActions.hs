module Game.Innovation.Rules.AdminActions
    where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.Maybe
import           Control.Monad
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import           Game.MetaGame
import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import           Game.Innovation.Rules.Helper
import           Game.Innovation.Rules.CoreActions


setDeck :: Map Age DrawStack -> Move Board
setDeck deck = M $ S.modify (\board -> board{ _drawStacks=deck })

drawDominations :: Move Board
drawDominations = M $ mapM_ (\age -> do
                                (d,ds) <- S.gets (popCards 1 . fromJust . Map.lookup age . L.view L.drawStacks)
                                S.modify (L.over L.dominateables (pushCards d))
                                S.modify (L.over L.drawStacks (Map.insert age ds))) ages

shuffle :: Seed -> Move Board
shuffle seed = M $ do
  logAnEntry ("Shuffle with " <<> view seed)
  S.modify (shuffleState seed)

addPlayer :: String -> Move Board
addPlayer playerId = M $ do
  log ("Add player: " ++ playerId)
  mstate <- S.gets _machineState
  case mstate of
    Prepare -> do
      let newPlayer = mkPlayer playerId
      S.modify (L.players L.%~ (newPlayer :))
    _       -> logError "not in prepare state."

determineInitialPlayerOrder :: Move Board
determineInitialPlayerOrder = let
  finalizeInitialPlayerOrder [p1,p2]       = [p1,p2,p2]
  finalizeInitialPlayerOrder [p1,p2,p3]    = [p1,p2,p2,p3,p3]
  finalizeInitialPlayerOrder [p1,p2,p3,p4] = [p1,p2,p3,p3,p4,p4]
  finalizeInitialPlayerOrder _             = error "in finalizeInitialPlayerOrder"
  in M $ do
    ps <- L.use L.players
    let playedCards = map (\p -> let
                              playedCard = getActiveCards p
                              in (getUId p,playedCard)) ps
    uidWithInitialCard <- mapM (\(p,cs) -> do
                                   when (length cs /= 1)
                                     (logError $ "Player " ++ show p ++ "should have played exactly one card")
                                   return (p, head cs)
                               ) playedCards
    let minimalCard = minimum (map snd uidWithInitialCard)
    let prefix = takeWhile (\(_,c) -> c /= minimalCard) uidWithInitialCard
    let postfix = dropWhile (\(_,c) -> c /= minimalCard) uidWithInitialCard
    L.playerOrder L..= finalizeInitialPlayerOrder (map fst (postfix ++ prefix)) -- FALSE! / TODO

handOutInitialCards :: Move Board
handOutInitialCards = do
  M $ do
     ps <- L.use L.players
     Admin `loggs` "determin first cards"
     initialCards <- mapM ((\uid -> fmap (\l -> (uid,l))
                                         (uid `takes` drawNOfAnd 2 Age1)) . getUId)
                          ps
     Admin `loggs` "ask for first card"
     answeredQuestions <- mapM (\(uid,cs) -> fmap (\[c] -> (uid, List.partition (==c) cs))
                                                  ((uid `chooseOneOf` "the cards, to be played out first") cs))
                               initialCards
     Admin `loggs` "all questions for first card were answered, play them"
     mapM_ (\(uid,(playCards,handCards)) -> do
               uid `takes` putIntoPlay playCards
               uid `takes` putIntoHand handCards) answeredQuestions
  determineInitialPlayerOrder
  M $ L.machineState L..= WaitForTurn

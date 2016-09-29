{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.MetaGame.Types
       ( PrettyPrint (..)
       , UserId (..), mkUserId, isAdmin
       , PlayerC (..)
       , BoardC (..), GameResult (..)
       , Log (..), clog, viewLog
       , InnerMoveType, InnerMoveResult, runInnerMoveType
       , MoveType, MoveResult, runMoveType
       , MoveWR (..), Move (..)
       , ActionWR (..), Action (..), takes
       , ActionToken (..), unpackToken
       , Turn (..), does', runTurn
       , turnToMove
       , Choice (..)
       , HistoryItem (..), History (..)
       , Game (..), mkG, (<=>)
       )
       where

import           Prelude hiding (log)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Maybe
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (State, StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

-- | PrettyPrint is for printing things for user output or for rougth debugging

class PrettyPrint a where
  pp :: a -> String
  putpp :: a -> IO ()
  putpp = putStrLn . pp

--------------------------------------------------------------------------------
-- ** Users and user-related stuff

-- | should remove:
--   - newlines,
--   - whitespace at all,
-- and keep
--   - only alphanumeric + - _ , ...
sanitizeUserId :: String -> String
sanitizeUserId = id -- TODO

data UserId = U String
            | Admin
            deriving (Show,Eq,Read)
mkUserId :: String -> UserId
mkUserId = U . sanitizeUserId

instance PrettyPrint UserId where
  pp Admin   = "Admin"
  pp (U uid) = uid

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

class PlayerC player where
  getUId :: player -> UserId
  hasUId :: player -> UserId -> Bool
  hasUId player uid = getUId player == uid
  hasEqualUId :: player -> player -> Bool
  hasEqualUId player1 player2 = player1 `hasUId` getUId player2

--------------------------------------------------------------------------------
-- ** State

data GameResult = NoWinner
                | Winner UserId
                deriving (Show,Eq,Read)

class BoardC board where
  emptyBoard :: board

  -- | get the player, which is expected to act next
  -- if another user is acting, the game fails
  getCurrentPlayer' :: board -> UserId

  -- | the current player is done with its action
  advancePlayerOrder :: board -> board

  -- | atomic update
  -- this should check for winning conditions and do all other checks, which
  -- depend on the current state and could happen in mid turn
  doAtomicUpdate :: board -> board

  -- | unpack the currently known result from the board
  -- If there is a winner, the board should know about it
  getGameResult :: board -> GameResult

  -- | determine whether the game has already a winner and thus is ended
  hasWinner :: board -> Bool
  hasWinner board = case getGameResult board of
    NoWinner -> False
    Winner _ -> True

--------------------------------------------------------------------------------
-- ** Log

-- | A user dependent Log
newtype Log = Log (UserId -> Text)

clog :: String -> Log
clog = Log . const . T.pack

-- | helper function to get the log from the view of an user
viewLog :: UserId -> Log -> Text
viewLog userId (Log log) = log userId

-- | we can combine logs to in an monoidal way
instance Monoid Log where
  mempty                    = clog ""
  mappend (Log l1) (Log l2) = Log $ \userId -> let
    lo1 = l1 userId
    lo2 = l2 userId
    sep = T.pack $ if (not . T.null) lo1 && (not . T.null) lo2
                   then "\n"
                   else ""
    in T.concat [ l1 userId, sep ,l2 userId ]

--------------------------------------------------------------------------------
-- ** Moves
-- A move is the actual change on the board

-- | The type of an move
type InnerMoveType board = ExceptT Text -- ^ uses ExceptT to communicate failures
                           ( WriterT Log -- ^ uses WriterT to log
                                     ( State (Game board) -- ^ the history of the game
                                     )
                           )

-- | the result of a move is the resulting state together with a bunch of metadata
type InnerMoveResult board r = ( ( Either Text -- ^ this maybe contains the error text
                                           r -- ^ this is the calculated result
                                 , Log ) -- ^ this contains the log
                               , Game board ) -- ^ the history of the game

runInnerMoveType :: BoardC board =>
                    InnerMoveType board a -> InnerMoveResult board a
runInnerMoveType imt = (S.runState .
                        W.runWriterT .
                        E.runExceptT ) imt (G [])

-- | The type of an move
type MoveType board = StateT board -- ^ uses StateT to handle the state of the board as 'board'
                      ( InnerMoveType board )

-- | the result of a move is the resulting state together with a bunch of metadata
type MoveResult board r = ( ( Either Text -- ^ this maybe contains the error text
                                     ( r -- ^ this is the calculated result
                                     , board ) -- ^ this is the state of the board at the end of the calculation
                            , Log ) -- ^ this contains the log
                          , Game board ) -- ^ the history of the game

-- | Something of MoveType can be applied to an inital board state
runMoveType :: board -> MoveType board a -> MoveResult board a
runMoveType initial move = (S.runState .
                            W.runWriterT .
                            E.runExceptT .
                            S.runStateT move) initial (G [])

-- | The wrapper for a move
-- a 'MoveWR' is a 'Move' which returns something
newtype MoveWR board r = M {unpackMove :: MoveType board r}
-- | a 'Move' does not calculate anything, it just modifies the state (+ failures + log)
type Move board = MoveWR board ()

instance BoardC board =>
         Monoid (Move board) where
  mempty                = M $ S.modify id -- TODO
  mappend (M t1) (M t2) = M $ t1 >> t2

instance BoardC board =>
         Functor (MoveWR board) where
  fmap f move = do
        result <- move
        return (f result)

instance BoardC board =>
         Applicative (MoveWR board) where
  pure r      = M $ return r
  (M getF) <*> (M getX) = M $ do
    r <- getF
    x <- getX
    return $ r x

instance BoardC board =>
         Monad (MoveWR board) where
  return t    = M $ return t
  (M t) >>= f = M $ t >>= (unpackMove . f)

--------------------------------------------------------------------------------
-- ** Actions
-- An action is something a player can take and it results in a move on the board

newtype ActionWR board r = A { unpackAction :: UserId -> MoveWR board r }
type Action board = ActionWR board ()

instance BoardC board =>
         Monoid (Action board) where
  mempty                = A $ const mempty
  mappend (A a1) (A a2) = A $ \uid -> a1 uid <> a2 uid

instance BoardC board =>
         Functor (ActionWR board) where
  fmap f action = do
        result <- action
        return (f result)

instance BoardC board =>
         Applicative (ActionWR board) where
  pure r = A $ const $ return r
  (A getF) <*> (A getX) = A $ \userId -> do
    f <- getF userId
    x <- getX userId
    return $ f x

instance BoardC board =>
         Monad (ActionWR board) where
  return t    = A $ const $ return t
  (A t) >>= f = A $ \userId -> t userId >>= ((\f -> f userId) . unpackAction . f)


takes :: BoardC board =>
         UserId -> ActionWR board a -> MoveType board a
takes uid act = unpackMove (unpackAction act uid)

--------------------------------------------------------------------------------
-- ** ActionTokens
-- ActionTokens are used to identify actions

-- | an actionToken is something which
--   - has a Read and a Show instance
--   - knows its corresponding action
class (BoardC board, Eq actionToken, Read actionToken, Show actionToken) =>
      ActionToken board actionToken where
  getAction :: actionToken -> Action board
  isAllowedFor :: actionToken -> UserId -> MoveWR board Bool
  isAllowedFor _ _ = return True

unpackToken :: ActionToken board actionToken =>
               actionToken -> UserId -> MoveWR board ()
unpackToken token userId = do
  b <- isAllowedFor token userId
  if b
    then unpackAction (getAction token) userId
    else M $ lift . E.throwE . T.pack $ "user " ++ pp userId ++ " is not allowed to " ++ show token

--------------------------------------------------------------------------------
-- ** Turns
-- A turn is the choice of an action, taken by an player

-- | A turn is a action which is taken by some player
data Turn board = forall actionToken.
                  ActionToken board actionToken =>
                  Turn { getActingPlayer :: UserId
                       , getActionToken :: actionToken }

instance Show (Turn board) where
  show (Turn Admin actionToken)      = show actionToken
  show (Turn (U userId) actionToken) = userId ++ ": " ++ show actionToken

-- | The `Eq` instance of `Action board` is deriven from the `Show` instance
instance Eq (Turn board) where
  turn1 == turn2 = getActingPlayer turn1 == getActingPlayer turn2
                   && show turn1 == show turn2

-- *** related helper
-- | redeem the described Move from an turn
turnToMove :: Turn board -> Move board
turnToMove (Turn userId actionToken) = unpackToken actionToken userId

-- | run a turn on a board
-- this also advances the player order, i.e. consumes an 'action'
runTurn :: BoardC board =>
           Turn board -> board -> InnerMoveType board board
runTurn turn b0 = do
  (_, b1) <- S.runStateT (unpackMove (turnToMove turn)) b0
  return $ advancePlayerOrder b1

--------------------------------------------------------------------------------
-- ** Choices

data Choice

--------------------------------------------------------------------------------
-- ** Game

data  HistoryItem board = HTurn (Turn board)
                        | HChoice Choice
instance Show (HistoryItem board) where
  show (HTurn t)   = show t
  show (HChoice _) = "some choice" -- TODO

type History board = [HistoryItem board]

does' :: ActionToken board actionToken =>
         Proxy board -> UserId -> actionToken -> HistoryItem board
does' _ uid t = HTurn $ Turn uid t

-- | A game consists of all the turns, i.e. taken actions, in chronological order
-- the last taken action is the head
newtype Game board = G (History board)
                   deriving (Show)

mkG :: [HistoryItem board] -> Game board
mkG = G . reverse

(<=>) :: Game board -> HistoryItem board -> Game board
(G g) <=> hi = G $ hi:g

instance Monoid (Game board) where
  mempty                = G []
  mappend (G g2) (G g1) = G $ mappend g1 g2

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.MetaGame
       ( IDAble (..)
       , UserId (..), UserC (..), isAdmin
       , BoardC (..), GameResult (..)
       , Log (..), viewLog
       , MoveType, Move (..)
       , ActionToken (..), Action (..)
       , Turn (..), does'
       , Game (..)
       , turnToMove, turnsToMove
       , play, extractGameResult, extractLog, extractBoard
       , getCurrentPlayer
       , log, logForMe, logError
       , onlyAdminIsAllowed
       ) where

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
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

class (Eq id, Show id, Read id) =>
      IDAble id a where
  getId :: a -> id
  hasId :: a -> id -> Bool
  hasId a id = getId a == id
-- instance Eq (IDAble id a) where
--   a1 == a2 = hasId a1 $ getId a2

--------------------------------------------------------------------------------
-- ** Users and user-related stuff

isValidUserId :: String -> Bool
isValidUserId id = True -- TODO

data UserId = U String
            | Admin
            deriving (Show,Eq,Read)

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

type UserC user = IDAble UserId user

--------------------------------------------------------------------------------
-- ** State

data GameResult = NoWinner
                | WinningOrder [UserId]
                deriving (Show,Eq,Read)

class BoardC board where
  emptyBoard :: board

  -- | get the player, which is expected to act next
  -- if another user is acting, the game fails
  getCurrentPlayer' :: board -> UserId

  -- | the current player is done with its action
  advancePlayerOrder :: board -> board

  -- | determineWinner
  determineGameResult :: board -> board

  -- | unpack the currently known result from the board
  getGameResult :: board -> GameResult

instance BoardC board =>
         BoardC (Game board, board) where
  emptyBoard          = (G [], emptyBoard)
  getCurrentPlayer'   = getCurrentPlayer' . L.view L._2
  advancePlayerOrder  = L.over L._2 advancePlayerOrder
  determineGameResult = L.over L._2 determineGameResult
  getGameResult       = getGameResult . L.view L._2

--------------------------------------------------------------------------------
-- ** Log

-- | A user dependent Log
newtype Log = Log (UserId -> Text)

-- | helper function to get the log from the view of an user
viewLog :: UserId -> Log -> Text
viewLog userId (Log log) = log userId

-- | we can combine logs to in an monoidal way
instance Monoid Log where
  mempty                    = Log $ const (T.pack "")
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
type MoveType board r = StateT board -- ^ uses StateT to handle the state of the board as 'board'
                        ( ExceptT Text -- ^ uses ExceptT to communicate failures
                          ( WriterT Log -- ^ uses WriterT to log
                                    Identity ) )
                        r -- ^ the value which is calculated

-- | the result of a move is the resulting state together with a bunch of metadata
type MoveResult board r = ( Either Text -- ^ this maybe contains the error text
                            ( r -- ^ this is the calculated result
                            , board ) -- ^ this is the state of the board at the end of the calculation
                          , Log ) -- ^ this contains the log

-- | Something of MoveType can be applied to an inital board state
runMoveType :: board -> MoveType board a -> MoveResult board a
runMoveType initial move = (runIdentity .
                            W.runWriterT .
                            E.runExceptT .
                            S.runStateT move) initial

-- | The wrapper for a move
-- a move does not calculate anything, it just modifies the state (+ failures + log)
newtype Move board = M {unpackMove :: MoveType board ()}

instance BoardC board =>
         Monoid (Move board) where
  mempty                = M $ S.modify id -- TODO: make easyier
  mappend (M t1) (M t2) = M $ t1 >> t2

--------------------------------------------------------------------------------
-- ** Actions
-- An action is something a player can take and it results in a move on the board

newtype Action board = A { unpackAction :: UserId -> Move board }

instance BoardC board =>
         Monoid (Action board) where
  mempty                = A $ const mempty
  mappend (A a1) (A a2) = A $ \uid -> a1 uid <> a2 uid

--------------------------------------------------------------------------------
-- ** ActionTokens
-- ActionTokens are used to identify actions

-- | an actionToken is something which
--   - has a Read and a Show instance
--   - knows its corresponding action
class (BoardC board, Eq actionToken, Read actionToken, Show actionToken) =>
      ActionToken board actionToken where
  getAction :: actionToken -> Action board

--------------------------------------------------------------------------------
-- ** Turns
-- A turn is the choice of an action, taken by an player

-- | A turn is a action which is taken by some player
data Turn board = forall actionToken.
                  ActionToken board actionToken =>
                  Turn { getActingPlayer :: UserId
                       , getActionToken :: actionToken }

does' :: ActionToken board actionToken =>
         Proxy board -> UserId -> actionToken -> Turn board
does' _ = Turn

instance Show (Turn board) where
  show (Turn Admin actionToken)      = show actionToken
  show (Turn (U userId) actionToken) = userId ++ ": " ++ show actionToken

-- | The `Eq` instance of `Action board` is deriven from the `Show` instance
instance Eq (Turn board) where
  act1 == act2 = show act1 == show act2

-- *** related helper
-- | redeem the described Move from an turn
turnToMove :: Turn board -> Move board
turnToMove (Turn userId actionToken) = (unpackAction $ getAction actionToken) userId

turnsToMove :: BoardC board =>
               [Turn board] -> Move board
turnsToMove = mconcat . map turnToMove

--------------------------------------------------------------------------------
-- ** Game

-- | A game consists of all the turns, i.e. taken actions, in chronological order
-- the last taken action is the head
newtype Game state = G [Turn state]
                   deriving (Show)

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

getGameResultM :: BoardC s =>
                  MoveType s GameResult
getGameResultM = S.gets getGameResult

-- | take an action and wrap it to also take care of
--  - the game history
--  - some rules:
--    - a game which is already finished should not be modified
--    - only the current user is allowed to act
actionToWrappedMove :: BoardC b =>
                       Turn b -> Move b
actionToWrappedMove turn = let
  -- | A finished game should not be modified
  onlyIfGameHasNotYetFinished :: BoardC s =>
                                 MoveType s ()
  onlyIfGameHasNotYetFinished = do
    gameResult <- getGameResultM
    unless (gameResult == NoWinner) $
      logError $ "the game is already over"

  -- | Only the current player (determined by the board-state) is allowed to act
  -- (or Admin)
  onlyCurrentUserIsAllowedToAct :: BoardC s =>
                                   MoveType s ()
  onlyCurrentUserIsAllowedToAct = do
    let actingUser = getActingPlayer turn
    currentPlayer <- getCurrentPlayer
    unless (actingUser == currentPlayer || isAdmin actingUser) $
      logError $ "the user " ++ show actingUser ++ " is not allowed to take an turn"

  in M $ do
    onlyIfGameHasNotYetFinished
    onlyCurrentUserIsAllowedToAct
    unpackMove $ turnToMove turn
    S.modify determineGameResult

actionsToWrappedMove :: BoardC board =>
                              [Turn board] -> Move board
actionsToWrappedMove = mconcat . map actionToWrappedMove

type PlayResult board = MoveResult board GameResult

-- | A game can be played and the resulting board as 'PlayResult board' is returned
play :: BoardC board =>
        Game board -> PlayResult board
play (G game) = runMoveType emptyBoard $ do
  unpackMove $ actionsToWrappedMove game
  getGameResultM

--------------------------------------------------------------------------------
-- ** related helper

extractGameResult :: PlayResult board -> GameResult
extractGameResult (Right (result,_),_) = result
extractGameResult _                    = NoWinner

extractLog :: PlayResult board -> Log
extractLog (_,log) = log

extractBoard :: PlayResult board -> Maybe board
extractBoard (Right (_,board),_) = Just board
extractBoard _                   = Nothing

--------------------------------------------------------------------------------
-- * Other helper for working in the monad
--------------------------------------------------------------------------------

-- ** helper for logging
log :: BoardC s =>
       String -> MoveType s ()
log text = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell .
      Log .
      const .
      T.pack $
      show loggingUser ++ ": " ++ text

logForMe :: BoardC s =>
            String -> String -> MoveType s ()
logForMe textPrivate textPublic = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell . Log $
      \user -> T.pack $
               ((show loggingUser ++ ": ") ++) $
               if user == loggingUser || user == Admin
               then textPrivate
               else textPublic

logError :: BoardC s =>
            String -> MoveType s a
logError error = do
  lift . lift . W.tell . Log . const . T.pack $ "Error: " ++ error
  lift . E.throwE $ T.pack error

-- ** helper for defining Actions and Moves

getCurrentPlayer :: BoardC s =>
                    MoveType s UserId
getCurrentPlayer = S.gets getCurrentPlayer'

onlyAdminIsAllowed :: BoardC board =>
                      Action board -> Action board
onlyAdminIsAllowed (A t) = A $ \case
  Admin -> t Admin
  _     -> M $ logError "Action was not authorized"

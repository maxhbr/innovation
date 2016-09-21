{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.MetaGame
       ( IDAble (..)
       , UserId (..), UserC (..), isAdmin
       , BoardC (..), GameResult (..)
       , Log (..), viewLog
       , TransitionType, Transition (..)
       , ActionC (..), Action
       , Game (..)
       , actionToTransition
       , play, extractGameResult, extractLog, extractState
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

  -- | unpack the currently known result from the board
  getGameResult' :: board -> GameResult

  -- | Helper function, which uses 'getGameResult'', is used to modify the state monad
  getBoardResult :: board -> (GameResult, board)
  getBoardResult s = (getGameResult' s, s)

instance BoardC board =>
         BoardC (Game board, board) where
  emptyBoard         = (G [], emptyBoard)
  getCurrentPlayer'  = getCurrentPlayer' . L.view L._2
  advancePlayerOrder = L.over L._2 advancePlayerOrder
  getGameResult'      = getGameResult' . L.view L._2

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
-- ** Transitions

-- | The type of an transition
--    * uses WriterT to log
--    * uses ExceptT to communicate failures
--    * uses StateT to handle the state '(Game s, s)' where
--      - the first thing represents the history of the game
--      - the second one is the current state of the board
type TransitionType s r = StateT (Game s, s) (ExceptT Text (WriterT Log Identity)) r
type TransitionResult s r = (Either Text (r, (Game s, s)) , Log)

-- | The wrapper for a transition
newtype Transition s = T { unpackTransition :: TransitionType s () }

runTransitionType :: s -> TransitionType s a -> TransitionResult s a
runTransitionType initial' transition' = (runIdentity .
                                          W.runWriterT .
                                          E.runExceptT .
                                          S.runStateT transition') (G [], initial')

runTransition :: s -> Transition s -> TransitionResult s ()
runTransition initial (T transition) = runTransitionType initial transition

-- | Transitions can be combined in an monoidal way
instance BoardC board =>
         Monoid (Transition board) where
  mempty                = T $ S.modify id -- TODO: make easyier
  mappend (T t1) (T t2) = T $ t1 >> t2

--------------------------------------------------------------------------------
-- ** ActionTokens and Actions
--  - it is described by the resulting 'Transition' on the 'board'
--  - An action knows its acting player
--  - it is referenced by a token

class (BoardC board, Read actionToken, Show actionToken) =>
      ActionC board actionToken where
  does :: UserId -> actionToken -> Transition board

-- | 'Action state' is an action which can be used in a game over 'state'
data Action state = forall actionToken.
                    ActionC state actionToken =>
                    Action { getActingPlayer :: UserId
                           , getToken :: actionToken }

instance Show (Action state) where
  show (Action Admin actionToken)      = show actionToken
  show (Action (U userId) actionToken) = userId ++ ": " ++ show actionToken

-- | The `Eq` instance of `Action state` is deriven from the `Show` instance
instance Eq (Action state) where
  act1 == act2 = show act1 == show act2

-- -- | this is the function by which actions should be created
-- --
-- -- by defining 'does = does\' (Proxy :: BoardStateData)' one is able to write
-- -- something like
-- --
-- -- @
-- --   game = G [ Admin `does` Init
-- --            , U "user1" `does` Play card1 ]
-- -- @
-- does' :: ActionC state actionToken =>
--          Proxy state -> UserId -> actionToken -> Action state
-- does' _ = Action

-- | redeem the described Transition from an action
actionToTransition :: Action state -> Transition state
actionToTransition (Action userId actionToken) = userId `does` actionToken

--------------------------------------------------------------------------------
-- ** Game

-- | A game consists of all the taken actions in chronological order
-- the last taken action is the head
newtype Game state = G [Action state]

--------------------------------------------------------------------------------
-- * play
--------------------------------------------------------------------------------

actionToWrappedTransition :: BoardC s =>
                Action s -> TransitionType s GameResult
actionToWrappedTransition action = let
  actingUser = getActingPlayer action

  -- | A finished game should not be modified
  onlyIfGameHasNotYetFinished :: BoardC s =>
                                 TransitionType s ()
  onlyIfGameHasNotYetFinished = do
    gameResult <- getGameResult
    unless (gameResult == NoWinner) $
      logError $ "the game is already over"

  -- | Only the current player (determined by the board-state) is allowed to act
  -- (or Admin)
  onlyCurrentUserIsAllowedToAct :: BoardC s =>
                                   TransitionType s ()
  onlyCurrentUserIsAllowedToAct = do
    currentPlayer <- getCurrentPlayer
    unless (actingUser == currentPlayer || isAdmin actingUser) $
      logError $ "the user " ++ show actingUser ++ " is not allowed to take an action"

  in do
    onlyIfGameHasNotYetFinished
    onlyCurrentUserIsAllowedToAct
    unpackTransition $ actionToTransition action
    getGameResult

actionsToWrappedTransition :: BoardC s =>
                [Action s] -> TransitionType s GameResult
actionsToWrappedTransition = msum . map actionToWrappedTransition

type PlayResult state = (Either Text -- ^ the potential error text
                                ( GameResult -- ^ the result of the game, extracted from the state
                                , state) -- ^ the final state of the game
                        , Log) -- ^ the log of the game

-- | A game can be played and the resulting state as 'PlayResult state' is returned
play :: BoardC state =>
        Game state -> PlayResult state
play (G game) = let

  stripHistory :: PlayResult (Game state, state) -> PlayResult state
  stripHistory (Right (res,(_,state)), log) = (Right (res, state), log)
  stripHistory (Left err,              log) = (Left err,           log)

  in stripHistory $
     runTransitionType emptyBoard $
     actionsToWrappedTransition game

--------------------------------------------------------------------------------
-- ** related helper

extractGameResult :: PlayResult state -> GameResult
extractGameResult (Right (result,_),_) = result
extractGameResult _                    = NoWinner

extractLog :: PlayResult state -> Log
extractLog (_,log) = log

extractState :: PlayResult state -> Maybe state
extractState (Right (_,state),_) = Just state
extractState _                   = Nothing

--------------------------------------------------------------------------------
-- * Other helper for working in the monad
--------------------------------------------------------------------------------

getGameResult :: BoardC s =>
                 TransitionType s GameResult
getGameResult = S.gets getGameResult'

-- ** helper for logging
log :: BoardC s =>
       String -> TransitionType s ()
log text = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell .
      Log .
      const .
      T.pack $
      show loggingUser ++ ": " ++ text

logForMe :: BoardC s =>
            String -> String -> TransitionType s ()
logForMe textPrivate textPublic = do
    loggingUser <- getCurrentPlayer
    lift . lift . W.tell . Log $
      \user -> T.pack $
               ((show loggingUser ++ ": ") ++) $
               if user == loggingUser || user == Admin
               then textPrivate
               else textPublic

logError :: BoardC s =>
            String -> TransitionType s a
logError error = do
  lift . lift . W.tell . Log . const . T.pack $ "Error: " ++ error
  lift . E.throwE $ T.pack error

-- ** helper for defining Transactions and Transaction like things

getCurrentPlayer :: BoardC s =>
                    TransitionType s UserId
getCurrentPlayer = S.gets getCurrentPlayer'


onlyAdminIsAllowed :: UserId -> Transition state ->  Transition state
onlyAdminIsAllowed Admin t = t
onlyAdminIsAllowed _     _ = T $ lift . E.throwE $ T.pack "Action was not authorized"

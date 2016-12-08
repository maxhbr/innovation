{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Game.MetaGame.Types.Game
       ( PlayerC
       , InnerMoveType, InnerMoveResult, runInnerMoveType
       , OuterMoveResult, liftFromInner, runOuterMoveType
       , MoveType, MoveResult
       , getMachineState, setMachineState, getCurrentPlayer, setCurrentPlayer
       , getObject, setObject, modifyObject
       , MoveWR (..), Move
       , ActionType, runActionType
       , ActionWR (..), Action, takes
       , ActionToken (..)
       , Turn (..)
       , Game (..)
       ) where

import           Prelude hiding (log)
import           Data.Monoid
import           Data.Text (Text)
import           Control.Monad.Trans
import           Control.Monad.Trans.Writer (Writer, WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S

import           Game.MetaGame.Types.Core hiding (getObject, setObject, modifyObject)
import           Game.MetaGame.Types.GameState hiding (getObject, setObject, modifyObject, getMachineState, setMachineState, getCurrentPlayer, setCurrentPlayer)
import qualified Game.MetaGame.Types.GameState as GS

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

class (IdAble player) =>
      PlayerC player

--------------------------------------------------------------------------------
-- ** Moves
-- A move is the actual change on the board

type InnerMoveType
  = ExceptT Text -- ^ uses ExceptT to communicate failures
    ( WriterT Log -- ^ uses WriterT to log
      ( Writer Game ) ) -- ^ the history of the game

type InnerMoveResult r
  = ( ( Either Text -- ^ this maybe contains the error text
        r -- ^ this is the calculated result
      , Log ) -- ^ this contains the log
    , Game ) -- ^ the history of the game

runInnerMoveType :: InnerMoveType a -> InnerMoveResult a
runInnerMoveType = W.runWriter . W.runWriterT . E.runExceptT

type MoveType
  = StateT GameState -- ^ uses StateT to handle the state of the board end everything else
           ( InquiryLayer InnerMoveType )

liftFromInner :: InnerMoveType a -> MoveType a
liftFromInner = lift . lift . lift

type OuterMoveResult r
  = InquiryResult ( r -- ^ this is the calculated result
                  , GameState ) -- ^ this is the state of the board at the end of the calculation

type MoveResult r = InnerMoveResult (OuterMoveResult r)

runOuterMoveType :: GameState -> [Answer] -> MoveType r -> InnerMoveType (OuterMoveResult r)
runOuterMoveType startState as move = E.runExceptT
                                      ( S.runStateT
                                        ( S.runStateT move startState)
                                        as )

-- | Something of MoveType can be applied to an inital board state
runMoveType :: GameState -> [Answer] -> MoveType a -> MoveResult a
runMoveType gameState cs = runInnerMoveType . runOuterMoveType gameState cs

-- | The wrapper for a move
-- a 'MoveWR' is a 'Move' which returns something
newtype MoveWR r
  = M {unpackMove :: MoveType r}
-- | a 'Move' does not calculate anything, it just modifies the state (+ failures + log)
type Move
  = MoveWR ()

runMove :: GameState -> [Answer] -> MoveWR a -> MoveResult a
runMove gameState as = runMoveType gameState as . unpackMove

instance Monoid Move where
  mempty                = M $ S.modify id -- TODO
  mappend (M t1) (M t2) = M $ t1 >> t2

instance Functor MoveWR where
  fmap f move = move >>= (return . f)

instance Applicative MoveWR where
  pure r = M $ return r
  (M getF) <*> (M getX) = M $ do
    r <- getF
    x <- getX
    return $ r x

instance Monad MoveWR where
  return t    = M $ return t
  (M t) >>= f = M $ t >>= (unpackMove . f)

--------------------------------------------------------------------------------
-- ** State helper

getMachineState :: MoveType MachineState
getMachineState = S.gets GS.getMachineState
setMachineState :: MachineState -> MoveType ()
setMachineState ms = S.modify (GS.setMachineState ms)

getCurrentPlayer :: MoveType UserId
getCurrentPlayer = S.gets GS.getCurrentPlayer
setCurrentPlayer :: UserId -> MoveType ()
setCurrentPlayer cp = S.modify (GS.setCurrentPlayer cp)

getObject :: IdAble a =>
        IdF a -> MoveType a
getObject idA = do
  maybeA <- S.gets (GS.getObject idA)
  case maybeA of
    Just a -> return a
    Nothing -> undefined -- TODO

setObject :: IdAble a =>
             a -> MoveType ()
setObject a = S.modify (GS.setObject a)

modifyObject :: IdAble a =>
             (a -> a) -> IdF a -> MoveType ()
modifyObject f idA = S.modify (GS.modifyObject f idA)

--------------------------------------------------------------------------------
-- ** Actions
-- An action is something a player can take and it results in a move on the board

type ActionType
  = ReaderT UserId -- ^ the user doing the action (also the logging user, ...)
            MoveType -- ^ the move behind the action

runActionType :: UserId -> ActionType r -> MoveType r
runActionType = flip R.runReaderT

newtype ActionWR r = A { unpackAction :: ActionType r }
type Action = ActionWR ()

takes :: UserId -> ActionWR r -> MoveType r
takes uid = runActionType uid . unpackAction

instance Monoid Action where
  mempty                = A (return mempty)
  mappend (A a1) (A a2) = A (a1 >> a2)

instance Functor ActionWR where
  fmap f action = action >>= (return . f)

instance Applicative ActionWR where
  pure r = A (return r)
  (A getF) <*> (A getX) = A (getF <*> getX)

instance Monad ActionWR where
  return t    = A (return t)
  (A t) >>= f = A (t >>= (unpackAction . f))


--------------------------------------------------------------------------------
-- ** ActionTokens
-- ActionTokens are used to identify actions

-- | an actionToken is something which
--   - has a Read and a Show instance
--   - knows its corresponding action
class (View actionToken, Eq actionToken, Read actionToken, Show actionToken) =>
      ActionToken actionToken where
  -- | returns the action corresponding to an Token
  getAction :: actionToken -> Action

  -- | returns, whether the board is within an state, where the turn can be applied
  stateMatchesExpectation :: actionToken -> MoveType Bool
  stateMatchesExpectation _ = do
    ms <- getMachineState
    return (ms == WaitForTurn)

--------------------------------------------------------------------------------
-- ** Turns
-- A turn is the choice of an action, taken by an player

-- | A turn is a action which is taken by some player
-- TODO: Might be better called 'Action' since "one has two actions per turn"
data Turn
  = forall actionToken.
    ActionToken actionToken =>
    Turn { getActingPlayer :: UserId
         , getActionToken :: actionToken
         , answers :: [Answer] }

instance Show Turn where
  show (Turn Admin actionToken choices)      = show actionToken ++ show choices
  show (Turn (U userId) actionToken choices) = userId ++ ": " ++ show actionToken ++ show choices
  show (Turn Guest _ _)                      = error "Guest is not allowed to have an turn"

instance View Turn where
  view (Turn uid actionToken choices) = chownLE uid ((view uid <>> ": ") <> view actionToken) -- <>> ("[" ++ show choices ++ "]")

-- | The `Eq` instance of `Action board` is deriven from the `Show` instance
instance Eq Turn where
  turn1 == turn2 = getActingPlayer turn1 == getActingPlayer turn2
                   && show turn1 == show turn2

--------------------------------------------------------------------------------
-- ** Game

type History
  = [Turn]

-- | A game consists of all the turns, i.e. taken actions, in chronological order
-- the last taken action is the head
newtype Game
  = G History
  deriving (Show)

instance Monoid Game where
  mempty                = G []
  mappend (G g2) (G g1) = G $ mappend g1 g2

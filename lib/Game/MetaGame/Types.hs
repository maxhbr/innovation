{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.MetaGame.Types
       ( Viewable (..)
       , UserId (..), mkUserId, isAdmin
       , PlayerC (..)
       , Inquiry (..), Answer (..)
       , BoardC (..), MachineState (..)
       , Log (..), clog, viewLog
       , InnerMoveType, InnerMoveResult, runInnerMoveType
       , OuterMoveResult, runOuterMoveType
       , MoveType, MoveResult, runMoveType
       , MoveWR (..), Move (..)
       , ActionWR (..), Action (..), takes
       , ActionToken (..)
       , Turn (..)
       , Game (..)
       , UserInput (..)
       , does', chooses', answerYes, answerNo
       , mkG, (<=>)
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
import           Control.Monad.Trans.Writer (Writer, WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (Except, ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

--------------------------------------------------------------------------------
-- ** Log

-- data LogLevel
--   = INFO
--   | ERROR
--   | NONE
--   deriving (Eq,Show,Enum,Ord,Bounded)

-- | A user dependent Log
newtype Log = Log (UserId -> Text)

clog :: String -> Log
clog = Log . const . T.pack

alog :: String -> Log
alog msg = Log (\case
                   Admin -> T.pack msg
                   _     -> T.pack "")

-- | helper function to get the log from the view of an user
viewLog :: UserId -> Log -> Text
viewLog userId (Log log) = log userId

insertLog :: Log -> (Text -> Text) -> Log
insertLog (Log l) f = Log $ \uid -> f (l uid)

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

newtype View a = View (UserId -> a)

-- | Viewable
--    rules:
--      restrict a == restrict (restrict a)
--      showViewable False a == showViewable False (restrict a)
class Viewable a where
  showViewable :: Bool -- ^ wheter viewer is owner or admin
               -> a -- ^ data to view
               -> String -- ^ result (used for log generation)

  -- | removes not-visible parts of the data, i.e. sets them to default
  restrict :: a -> a
  restrict = id

  mkView :: UserId -> a -> View a
  mkView owner a = let
    v uid | uid `isEqOrAdmin` owner = a
          | otherwise               = restrict a
    in View v

  logV :: UserId -> a -> Log
  logV owner a = let
    l uid | uid `isEqOrAdmin` owner = showViewable True a
          | otherwise               = showViewable False (restrict a)
    in Log $ T.pack . l

  -- mkView :: Bool -> a -> a
  -- mkView True  = id
  -- mkView False = restrict

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

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

instance Viewable UserId where
  showViewable _ Admin   = "Admin"
  showViewable _ (U uid) = uid

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

isEqOrAdmin :: UserId -- ^ the asking user
            -> UserId -- ^ the user who is to be matched
            -> Bool
isEqOrAdmin Admin _   = True
isEqOrAdmin uid owner = owner == uid

class PlayerC player where
  getUId :: player -> UserId
  hasUId :: player -> UserId -> Bool
  hasUId player uid = getUId player == uid
  hasEqualUId :: player -> player -> Bool
  hasEqualUId player1 player2 = player1 `hasUId` getUId player2

--------------------------------------------------------------------------------
-- ** Choices
-- While evaluating a turn, there might be the point where user input by an
-- specific user is required. This question is represented by an 'Inquiry'.
-- This can be answerd by something of the type 'Choice'.

data Inquiry a
  = Inquiry { askedPlayer :: UserId -- ^ the user asked to answer the question
            , inquiryQuestion :: String -- ^ the verbal version of the question
            , inquiryOptions :: [a] -- ^ possible options to choose from
            , checkConfigurationValiadity :: [Int] -> Bool } -- ^ restrictions to the possible answers (programatically)

instance Eq a =>
         Eq (Inquiry a) where
  (Inquiry ap1 iq1 io1 _) == (Inquiry ap2 iq2 io2 _) = iq1 == iq2
                                                    && io1 == io2
                                                    && ap1 == ap2

instance Viewable a =>
         Viewable (Inquiry a) where
  restrict (Inquiry ap iq _ _) = Inquiry ap iq [] (const True)
  showViewable isAllowed inq@(Inquiry ap iq io _) = iq ++ " <- " ++ showViewable isAllowed ap
                                             ++ " (Options are " ++ "TODO" ++ ")"

data Answer
  = Answer { answeringPlayer :: UserId -- ^ the user answering the inquiry
           , answer :: [Int] } -- ^ choose options by their indices (starting with 0)
                               -- '[]' means "No" for boolean questions, '[0]' is "Yes"
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- ** State

data MachineState
  = Prepare -- ^ the game has not yet been started and only 'Admin' should take actions
  | WaitForTurn -- ^ last turn was done completly and current player should choose his action
  | forall a.
    (Viewable a) =>
    WaitForChoice (Inquiry a) -- ^ current turn requires more imput, which needs to be provided by some user
  | GameOver UserId -- ^ there was already an game ending situation and mentioned user has won, nothing should be able to change the current board
  -- deriving (Show)

instance Eq MachineState where
  Prepare           == Prepare           = True
  WaitForTurn       == WaitForTurn       = True
  (WaitForChoice _) == (WaitForChoice _) = True
  (GameOver gr1)    == (GameOver gr2)    = gr1 == gr2
  _                 == _                 = False

instance Viewable MachineState where
  restrict (WaitForChoice inq) = WaitForChoice (restrict inq)
  restrict s                   = s

  showViewable _ Prepare             = "Prepare"
  showViewable _ WaitForTurn         = "WaitForTurn"
  showViewable b (WaitForChoice inq) = "WaitForChoice: " ++ (showViewable b inq)
  showViewable b (GameOver winner)   = "GameOver: " ++ (showViewable b winner) ++ " has won"

class BoardC board where
  emptyBoard :: board

  -- | metainformation on the state of the board
  getMachineState' :: board -> MachineState

  -- | set the metainfo
  setMachineState' :: MachineState -> board -> board

  -- | get the player, which is expected to act next (take a turn, give an answer, ...)
  -- if another user is acting, the game fails
  getCurrentPlayer' :: board -> UserId

  -- | the current turn is done
  advancePlayerOrder :: board -> board

  -- | atomic update
  -- this should check for winning conditions and do all other checks, which
  -- depend on the current state and could happen in mid turn
  doAtomicUpdate :: board -> Except Text board

  -- | unpack the currently known result from the board
  -- If there is a winner, the board should know about it
  getWinner :: board -> Maybe UserId
  getWinner b = case getMachineState' b of
    GameOver winner -> Just winner
    _               -> Nothing

  -- | determine whether the game has already a winner and thus is ended
  hasWinner :: board -> Bool
  hasWinner board = case getWinner board of
    Just _ -> True
    Nothing -> False

--------------------------------------------------------------------------------
-- ** Moves
-- A move is the actual change on the board

type InnerMoveType board
  = ExceptT Text -- ^ uses ExceptT to communicate failures
    ( WriterT Log -- ^ uses WriterT to log
      ( Writer ( Game board ) ) ) -- ^ the history of the game

type InnerMoveResult board r
  = ( ( Either Text -- ^ this maybe contains the error text
        r -- ^ this is the calculated result
      , Log ) -- ^ this contains the log
    , Game board ) -- ^ the history of the game

runInnerMoveType :: InnerMoveType board a -> InnerMoveResult board a
runInnerMoveType = W.runWriter . W.runWriterT . E.runExceptT

type MoveType board
  = StateT board -- ^ uses StateT to handle the state of the board as 'board'
    ( StateT [Answer] -- ^ a list of answers to consume (maybe implement via ReaderT)
      ( ExceptT board -- ^ the current turn could not be finished (some user input was missing)
        ( InnerMoveType board ) ) )

type OuterMoveResult board r
  = Either board -- ^ this is the fast way out, without ending the turn
                 -- the board should be in state 'WaitForChoice' or 'GameOver'
    ( ( r -- ^ this is the calculated result
      , board ) -- ^ this is the state of the board at the end of the calculation
    , [Answer] ) -- ^ this are the unconsumed answers

type MoveResult board r = InnerMoveResult board (OuterMoveResult board r)

runOuterMoveType :: board -> [Answer] -> MoveType board r -> InnerMoveType board (OuterMoveResult board r)
runOuterMoveType b0 as move = E.runExceptT
                              ( S.runStateT
                                ( S.runStateT move b0 )
                                as )

-- | Something of MoveType can be applied to an inital board state
runMoveType :: board -> [Answer] -> MoveType board a -> MoveResult board a
runMoveType b0 cs = runInnerMoveType . runOuterMoveType b0 cs

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

--------------------------------------------------------------------------------
-- ** Turns
-- A turn is the choice of an action, taken by an player

-- | A turn is a action which is taken by some player
-- TODO: Might be better called 'Action' since "one has two actions per turn"
data Turn board = forall actionToken.
                  ActionToken board actionToken =>
                  Turn { getActingPlayer :: UserId
                       , getActionToken :: actionToken
                       , answers :: [Answer] }

instance Show (Turn board) where
  show (Turn Admin actionToken choices)      = show actionToken ++ show choices
  show (Turn (U userId) actionToken choices) = userId ++ ": " ++ show actionToken ++ show choices

-- | The `Eq` instance of `Action board` is deriven from the `Show` instance
instance Eq (Turn board) where
  turn1 == turn2 = getActingPlayer turn1 == getActingPlayer turn2
                   && show turn1 == show turn2

--------------------------------------------------------------------------------
-- ** Game

type History board = [Turn board]

-- | A game consists of all the turns, i.e. taken actions, in chronological order
-- the last taken action is the head
newtype Game board = G (History board)
                   deriving (Show)

instance Monoid (Game board) where
  mempty                = G []
  mappend (G g2) (G g1) = G $ mappend g1 g2

--------------------------------------------------------------------------------
-- ** mkG

data UserInput board
  = UTurn (Turn board)
  | UChoice (Turn board -> Turn board)

does' :: ActionToken board actionToken =>
         Proxy board -> UserId -> actionToken -> UserInput board
does' _ uid t = UTurn $ Turn uid t []

chooses' :: Proxy board -> UserId -> (UserId -> Answer) -> UserInput board
chooses' _ uid c = UChoice $ \t -> t{ answers=c uid : answers t }

-- | possible answer (Yes)
answerYes :: UserId -> Answer
answerYes uid = uid `Answer` [0]

-- | possible answer (No)
answerNo :: UserId -> Answer
answerNo uid = uid `Answer` []

mkG :: [UserInput board] -> Game board
mkG = G . accumulateInput id
  where
    accumulateInput :: (Turn board -> Turn board) -> [UserInput board] -> [Turn board]
    accumulateInput cs (UChoice f : uis) = accumulateInput (f . cs) uis
    accumulateInput cs (UTurn t : uis)   = accumulateInput id uis ++ [cs t]
    accumulateInput _  []                = []

(<=>) :: Game board -> UserInput board -> Game board
(G (t:ts)) <=> (UChoice f) = G $ f t : ts
(G ts)     <=> (UTurn t)   = G $ t : ts

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.MetaGame.Types
       ( UserId (..), mkUserId, isAdmin
       , LogEntry (..), (<<>), (<>>)
       , Log (..), viewLog, logAnEntryI, logI, loggsI, logggsI, alogI
       , View (..)
       , PlayerC (..)
       , InqRestr (..), Inquiry (..), Answer (..)
       , BoardC (..), MachineState (..)
       , InnerMoveType, InnerMoveResult, runInnerMoveType
       , OuterMoveResult, runOuterMoveType
       , MoveType, MoveResult, runMoveType, runMove
       , MoveWR (..), Move (..)
       , ActionType (..), runActionType
       , ActionWR (..), Action (..), takes
       , ActionToken (..)
       , Turn (..)
       , Game (..)
       , UserInput (..)
       , does', chooses', answerYes, answerNo
       , mkG, (<=>)
       ) where

import           Prelude hiding (log)
import           Data.String
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
import           Control.Monad.Trans.Reader (Reader, ReaderT)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

--------------------------------------------------------------------------------
-- ** Users and user-related stuff

-- | should remove:
--   - newlines,
--   - whitespace at all,
-- and keep
--   - only alphanumeric + - _ , ...
sanitizeUserId :: String -> String
sanitizeUserId = id -- TODO

data UserId
  = U String -- ^ regular user, determined by its username
  | Admin -- ^ administrative user (is not allowed to play)
  | Guest -- ^ unauthorized user
  deriving (Show,Eq,Read)
mkUserId :: String -> UserId
mkUserId = U . sanitizeUserId

instance Ord UserId where
  compare Admin Admin   = EQ
  compare Admin (U _)   = LT
  compare (U _) Admin   = GT
  compare (U u1) (U u2) = compare u1 u2

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

getCommonUID :: UserId -> UserId -> UserId
getCommonUID Guest uid = uid
getCommonUID uid Guest = uid
getCommonUID Admin uid = Admin
getCommonUID uid Admin = Admin
getCommonUID uid1 uid2 = uid1 `getCommonUID` uid2

isAuthorizationLevel :: UserId -> UserId -> Bool
isAuthorizationLevel asker level = (asker `getCommonUID` level) == asker

--------------------------------------------------------------------------------
-- ** Log

-- data LogLevel
--   = INFO
--   | ERROR
--   | NONE
--   deriving (Eq,Show,Enum,Ord,Bounded)

-- type LogEntry
--   = Reader UserId
-- | a logentry will be a line of a log
data LogEntry
  -- | a log entry, which is visible for all users
  = CLogE Text -- ^ the text visible for all
  -- | a log entry, which also contains content which is only visible for the admin
  | ALogE Text -- ^ the text visible for all
          Text -- ^ the restricted text
  -- | a log entry containing hidden content, visible for specific users (and admin)
  | ULogE UserId
          Text -- ^ the text visible for all
          Text -- ^ the restricted text

canonifyLE :: LogEntry -> LogEntry
canonifyLE (ULogE Admin t1 t2) = canonifyLE (ALogE t1 t2)
canonifyLE (ULogE Guest _  t2) = CLogE t2
canonifyLE le@(ULogE _  t1 t2) | t1 == t2  = CLogE t1
                               | otherwise = le
canonifyLE le@(ALogE t1 t2)    | t1 == t2  = CLogE t1
                               | otherwise = le
canonifyLE le@(CLogE t)        = le

getRestricted, getUnrestricted :: LogEntry -> Text
getRestricted (CLogE t)     = t
getRestricted (ALogE _ t)   = t
getRestricted (ULogE _ _ t) = t
getUnrestricted (CLogE t)     = t
getUnrestricted (ALogE t _)   = t
getUnrestricted (ULogE _ t _) = t

-- decanonifyLE :: LogEntry -> LogEntry
-- decanonifyLE (CLogE t)     = ULogE Guest t t
-- decanonifyLE (ALogE t1 t2) = ULogE Admin t1 t2
-- decanonifyLE le@(ULogE{})  = le

viewLE :: UserId -> LogEntry -> Text
viewLE _      (CLogE t)         = t
viewLE Admin  (ALogE _ t2)      = t2
viewLE _      (ALogE t1 _)      = t1
viewLE viewer (ULogE uid t1 t2) | viewer `isAuthorizationLevel` uid = t2
                                | otherwise                         = t1

mkALogEntry :: String -> String -> LogEntry
mkALogEntry restr unrestr = canonifyLE (ALogE (T.pack restr) (T.pack unrestr))

mkUserLogEntry :: UserId -> String -> String -> LogEntry
mkUserLogEntry uid restr unrestr = canonifyLE (ULogE uid (T.pack restr) (T.pack unrestr))

instance Show LogEntry where
  show (CLogE t)     = T.unpack t
  show (ALogE t _)   = T.unpack t
  show (ULogE _ t _) = T.unpack t

instance IsString LogEntry where
  fromString = CLogE . T.pack

instance Monoid LogEntry where
  mempty = fromString ""
  mappend le1 le2 = canonifyLE $
                    ULogE (getOwner le1 `getCommonUID` getOwner le2)
                          (getUnrestricted le1 `T.append` getUnrestricted le2)
                          (getRestricted le1 `T.append` getRestricted le2)

(<<>) :: String -> LogEntry -> LogEntry
s <<> l = fromString s `mappend` l

(<>>) :: LogEntry -> String -> LogEntry
l <>> s = l `mappend` fromString s

-- newtype Log = Log (UserId -> Text)
-- data Log
--   = Log { _contentText :: Either Text -- ^ restricted text
--                                  Text -- ^ unrestricted text
--         , _isAllowed   :: UserId -> Bool } -- ^ Whether a user is allowed to see the unrestricted text
-- data Log
--   = Log { _getLogText      :: ( Text
--                               , Maybe Text )
--         , _isAllowedViewer :: UserId -> Bool } -- ^ Whether a user is allowed to see the unrestricted text
-- data Log
--   = Log Text -- ^ unrestricted text
--         Text -- ^ restricted text
--         [UserId] -- ^ list of authorized users (implicitliy always including Admin)
-- newtype Log
--   = Log (LogEntry)
-- | A user dependent Log
type Log = [LogEntry]

instance IsString Log where
  fromString s = [fromString s]

-- | helper function to get the log from the view of an user
viewLog :: UserId -> Log -> Text
viewLog userId log = T.unwords (map (cleanText . viewLE userId) log)

-- This may result in very bad performance
cleanText :: Text -> Text
cleanText t = let
  stripedText = T.strip t
  in if T.null stripedText
     then stripedText
     else stripedText `T.snoc` '\n'

-- *** logging helper for the inner circle

logAnEntryI :: (Monad m, MonadTrans t) =>
               LogEntry -> t (WriterT Log m) ()
logAnEntryI = lift
            . W.tell
            . (:[])

logI :: (Monad m, MonadTrans t) =>
        String -> t (WriterT Log m) ()
logI = logAnEntryI
     . fromString

loggsI :: (Monad m, MonadTrans t) =>
          UserId -> String -> t (WriterT Log m) ()
loggsI uid = logAnEntryI
           . ((show uid ++ ": ") <<>)
           . fromString

logggsI :: (Monad m, MonadTrans t) =>
          UserId -> String -> String -> t (WriterT Log m) ()
logggsI uid unrestricted = logAnEntryI
                         . ((show uid ++ ": ") <<>)
                         . mkUserLogEntry uid unrestricted

alogI :: (Monad m, MonadTrans t) =>
        String -> String -> t (WriterT Log m) ()
alogI unrestricted = logAnEntryI
                   . ((show Admin ++ ": ") <<>)
                   . mkALogEntry unrestricted

--------------------------------------------------------------------------------
-- * The View class
--------------------------------------------------------------------------------

class Show a =>
      View a where
  showRestricted :: a -> String
  showRestricted = show

  showUnrestricted :: a -> String
  showUnrestricted = show

  getOwner :: a -> UserId
  getOwner _ = Guest

  view :: a -> LogEntry
  view a = canonifyLE (ULogE (getOwner a)
                             ((T.pack . showUnrestricted) a)
                             ((T.pack . showRestricted) a))

instance View UserId

instance View LogEntry where
  showRestricted = T.unpack . getRestricted
  showUnrestricted = T.unpack . getUnrestricted
  getOwner CLogE{}         = Guest
  getOwner ALogE{}         = Admin
  getOwner (ULogE uid _ _) = uid
  view = id

--------------------------------------------------------------------------------
-- * Basic data and type declerations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Choices
-- While evaluating a turn, there might be the point where user input by an
-- specific user is required. This question is represented by an 'Inquiry'.
-- This can be answerd by something of the type 'Choice'.

newtype InqRestr
  = InqRestr ([Int] -> Bool)

instance Monoid InqRestr where
  mempty                                = InqRestr (const True)
  mappend (InqRestr ir1) (InqRestr ir2) = InqRestr (\cs -> ir1 cs && ir2 cs)

data Inquiry a
  = Inquiry { askedPlayer :: UserId -- ^ the user asked to answer the question
            , inquiryQuestion :: String -- ^ the verbal version of the question
            , inquiryOptions :: [a] -- ^ possible options to choose from
            , answerRestrictions :: InqRestr } -- ^ restrictions to the possible answers (programatically)

instance Eq a =>
         Eq (Inquiry a) where
  (Inquiry ap1 iq1 io1 _) == (Inquiry ap2 iq2 io2 _) = iq1 == iq2
                                                    && io1 == io2
                                                    && ap1 == ap2

instance Show a =>
         Show (Inquiry a) where
  show inq@(Inquiry ap iq io _) = iq ++ " <- " ++ show ap ++ " (Options are " ++ "TODO" ++ ")"
instance Show a =>
         View (Inquiry a) where
  getOwner (Inquiry ap _ _ _) = ap
  showRestricted inq@(Inquiry ap iq _ _) = iq ++ " <- " ++ show ap

data Answer
  = Answer { answeringPlayer :: UserId -- ^ the user answering the inquiry
           , answer :: [Int] } -- ^ choose options by their indices (starting with 0)
                               -- '[]' means "No" for boolean questions, '[0]' is "Yes"
  deriving (Show, Eq)
instance View Answer where
  getOwner (Answer ap _) = ap
  showRestricted (Answer ap _) = "[Answer by " ++ show ap ++ "]"

--------------------------------------------------------------------------------
-- ** State

data MachineState
  = Prepare -- ^ the game has not yet been started and only 'Admin' should take actions
  | WaitForTurn -- ^ last turn was done completly and current player should choose his action
  | forall a.
    (Show a) =>
    WaitForChoice (Inquiry a) -- ^ current turn requires more imput, which needs to be provided by some user
  | GameOver UserId -- ^ there was already an game ending situation and mentioned user has won, nothing should be able to change the current board

instance Eq MachineState where
  Prepare           == Prepare           = True
  WaitForTurn       == WaitForTurn       = True
  (WaitForChoice _) == (WaitForChoice _) = True
  (GameOver gr1)    == (GameOver gr2)    = gr1 == gr2
  _                 == _                 = False

instance Show MachineState where
  show Prepare             = "Prepare"
  show WaitForTurn         = "WaitForTurn"
  show (WaitForChoice inq) = "WaitForChoice: " ++ show inq
  show (GameOver winner)   = "GameOver: " ++ show winner ++ " has won"
instance View MachineState where
  getOwner (WaitForChoice inq) = getOwner inq
  getOwner _                   = Guest
  showRestricted (WaitForChoice inq) = "WaitForChoice: " ++ showRestricted inq
  showRestricted ms                  = show ms

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
newtype MoveWR board r
  = M {unpackMove :: MoveType board r}
-- | a 'Move' does not calculate anything, it just modifies the state (+ failures + log)
type Move board
  = MoveWR board ()

runMove :: board -> [Answer] -> MoveWR board a -> MoveResult board a
runMove b as = runMoveType b as . unpackMove

instance BoardC board =>
         Monoid (Move board) where
  mempty                = M $ S.modify id -- TODO
  mappend (M t1) (M t2) = M $ t1 >> t2

instance BoardC board =>
         Functor (MoveWR board) where
  fmap f move = move >>= (return . f)

instance BoardC board =>
         Applicative (MoveWR board) where
  pure r = M $ return r
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
type ActionType board
  = ReaderT UserId -- ^ the user doing the action (also the logging user, ...)
            ( MoveType board ) -- ^ the move behind the action

runActionType :: UserId -> ActionType board r -> MoveType board r
runActionType = flip R.runReaderT

newtype ActionWR board r = A { unpackAction :: ActionType board r }
type Action board = ActionWR board ()

takes :: UserId -> ActionWR board r -> MoveType board r
takes uid = runActionType uid . unpackAction

instance BoardC board =>
         Monoid (Action board) where
  mempty                = A (return mempty)
  mappend (A a1) (A a2) = A (a1 >> a2)

instance BoardC board =>
         Functor (ActionWR board) where
  fmap f action = action >>= (return . f)

instance BoardC board =>
         Applicative (ActionWR board) where
  pure r = A (return r)
  (A getF) <*> (A getX) = A (getF <*> getX)

instance BoardC board =>
         Monad (ActionWR board) where
  return t    = A (return t)
  (A t) >>= f = A (t >>= (unpackAction . f))


--------------------------------------------------------------------------------
-- ** ActionTokens
-- ActionTokens are used to identify actions

-- | an actionToken is something which
--   - has a Read and a Show instance
--   - knows its corresponding action
class (BoardC board, Eq actionToken, Read actionToken, Show actionToken) =>
      ActionToken board actionToken where
  -- | returns the action corresponding to an Token
  getAction :: actionToken -> Action board

  -- | returns, whether the board is within an state, where the turn can be applied
  stateMatchesExpectation :: actionToken -> MoveType board Bool
  stateMatchesExpectation _ = do
    ms <- S.gets getMachineState'
    return (ms == WaitForTurn)

--------------------------------------------------------------------------------
-- ** Turns
-- A turn is the choice of an action, taken by an player

-- | A turn is a action which is taken by some player
-- TODO: Might be better called 'Action' since "one has two actions per turn"
data Turn board
  = forall actionToken.
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

type History board
  = [Turn board]

-- | A game consists of all the turns, i.e. taken actions, in chronological order
-- the last taken action is the head
newtype Game board
  = G (History board)
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
mkG = G . accumulateInput id . reverse
  where
    accumulateInput :: (Turn board -> Turn board) -> [UserInput board] -> [Turn board]
    accumulateInput cs (UChoice f : uis) = accumulateInput (f . cs) uis
    accumulateInput cs (UTurn t : uis)   = cs t : accumulateInput id uis
    accumulateInput _  []                = []

(<=>) :: Game board -> UserInput board -> Game board
(G (t:ts)) <=> (UChoice f) = G $ f t : ts
(G ts)     <=> (UTurn t)   = G $ t : ts

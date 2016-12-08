{-# LANGUAGE ExistentialQuantification #-}
module Game.MetaGame.Types.GameState
       ( MachineState (..), GameState (..)
       , getMachineState, setMachineState, getCurrentPlayer, setCurrentPlayer
       , getObject, setObject, modifyObject
       , InqRestr (..)
       , Inquiry (..)
       , Answer (..)
       , InquiryLayer, InquiryResult
       , answerYes, answerNo
       ) where

import           Data.List
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.State.Lazy (StateT)

import           Game.MetaGame.Types.Core hiding (getObject, setObject, modifyObject)
import qualified Game.MetaGame.Types.Core as Core

--------------------------------------------------------------------------------
-- * MachineState and GameState
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

--------------------------------------------------------------------------------
-- ** GameState helper
data GameState
  = GameState
    { _world         :: World
    , _machineState  :: MachineState
    , _currentPlayer :: UserId }

getMachineState :: GameState -> MachineState
getMachineState = _machineState
setMachineState :: MachineState -> GameState -> GameState
setMachineState ms gs = gs{_machineState=ms}

getCurrentPlayer :: GameState -> UserId
getCurrentPlayer = _currentPlayer
setCurrentPlayer :: UserId -> GameState -> GameState
setCurrentPlayer cp gs = gs{_currentPlayer=cp}

getObject :: IdAble a =>
        IdF a -> GameState -> Maybe a
getObject idA = (Core.getObject idA) . _world

setObject :: IdAble a =>
             a -> GameState -> GameState
setObject a (GameState w ms cp) = GameState (Core.setObject a w) ms cp

modifyObject :: IdAble a =>
             (a -> a) -> IdF a -> GameState -> GameState
modifyObject f idA (GameState w ms cp) = GameState (Core.modifyObject f idA w) ms cp

--------------------------------------------------------------------------------
-- * Choices
-- While evaluating a turn, there might be the point where user input by an
-- specific user is required. This question is represented by an 'Inquiry'.
-- This can be answerd by something of the type 'Choice'.

-- ** the questions

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
  show inq@(Inquiry ap iq ios _) = let
    showIos = intercalate ", " (map show ios)
    in iq ++ " <- " ++ show ap ++ " (Options are " ++ showIos ++ ")"
instance Show a =>
         View (Inquiry a) where
  getOwner (Inquiry ap _ _ _) = ap
  showRestricted inq@(Inquiry ap iq _ _) = iq ++ " <- " ++ show ap

-- ** the answers

data Answer
  = Answer { answeringPlayer :: UserId -- ^ the user answering the inquiry
           , answer :: [Int] } -- ^ choose options by their indices (starting with 0)
                               -- '[]' means "No" for boolean questions, '[0]' is "Yes"
  deriving (Show, Eq)
instance View Answer where
  getOwner (Answer ap _) = ap
  showRestricted (Answer ap _) = "[Answer by " ++ show ap ++ "]"

type InquiryLayer a = StateT [Answer] -- ^ a list of answers to consume
                             ( ExceptT GameState a ) -- ^ the current turn could not be finished (some user input was missing)
type InquiryResult r
  = Either GameState -- ^ this is the fast way out, without ending the turn
                     -- the board should be in state 'WaitForChoice' or 'GameOver'
           ( r
           , [Answer] ) -- ^ this are the unconsumed answers

-- | possible answer (Yes)
answerYes :: UserId -> Answer
answerYes uid = uid `Answer` [0]

-- | possible answer (No)
answerNo :: UserId -> Answer
answerNo uid = uid `Answer` []

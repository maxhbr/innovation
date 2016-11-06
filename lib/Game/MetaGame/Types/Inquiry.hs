module Game.MetaGame.Types.Inquiry
       ( InqRestr (..)
       , Inquiry (..)
       , Answer (..), answerYes, answerNo
       ) where

import Data.List

import           Game.MetaGame.Types.Core

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

-- | possible answer (Yes)
answerYes :: UserId -> Answer
answerYes uid = uid `Answer` [0]

-- | possible answer (No)
answerNo :: UserId -> Answer
answerNo uid = uid `Answer` []

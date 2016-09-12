{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Machine
       ( UserId
       , UserAction, pack
       , isApplicable
       , Game, play)
    where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Writer

--------------------------------------------------------------------------------
-- Game state
--------------------------------------------------------------------------------

type UserId = String
type Log = Maybe UserId -> Text
type LogWriter s = Writer Log s

class (Read action, Show action) => UserActionC state action where
  getActingUser'      :: action -> UserId
  getLog'             :: action -> state -> Log
  isApplicable'       :: action -> state -> Bool
  getErrorMessage'    :: action -> state -> Text
  getStateTransition' :: action -> (state -> state)

data UserAction state = forall action. UserActionC state action => UserAction { getAction :: action }
pack :: UserActionC state action => Proxy state -> action -> UserAction state
pack _ = UserAction

#if false
instance forall action. (UserActionC state action) => Read (UserAction state) where
  -- readsPrec :: Int -> ReadS a
  readsPrec _ input = [(UserAction $ read input, "")]
#endif

instance Show (UserAction state) where
  show (UserAction state) = show state

instance Eq (UserAction s) where
  act1 == act2 = show act1 == show act2

type Game state = [UserAction state]

--------------------------------------------------------------------------------
-- helper functions, which unpack the functions of UserActionC
--------------------------------------------------------------------------------

getLog :: UserAction state -> state -> Log
getLog (UserAction action) = getLog' action

isApplicable :: UserAction state -> state -> Bool
isApplicable (UserAction action) = isApplicable' action

getErrorMessage :: UserAction state -> state -> Text
getErrorMessage (UserAction action) = getErrorMessage' action

getStateTransition :: UserAction state -> (state -> state)
getStateTransition (UserAction action) = getStateTransition' action

--------------------------------------------------------------------------------
-- play
--------------------------------------------------------------------------------

play :: state -> Game state -> (Either Text state, Log)
play state as = runWriter $ play' state as
  where
    play' :: state -> Game state -> LogWriter (Either Text state)
    play' state (a:as) = if isApplicable a state
                            then do
                              tell (getLog a state)
                              play' (getStateTransition a state) as
                            else do
                              let errorMessage = getErrorMessage a state
                              tell (const errorMessage)
                              pure $ Left errorMessage
    play' state []    = pure $ Right state

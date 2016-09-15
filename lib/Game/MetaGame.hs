{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Game.MetaGame
       ( UserId (..)
       , Log (..)
       , UserC (..)
       , StateC (..)
       , UserActionC (..), UserAction, does'
       , Game, play'
       , getLog, printLog)
    where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W

--------------------------------------------------------------------------------
-- Basic data and type declerations
--------------------------------------------------------------------------------

data UserId = U { unpackUserId :: String }
            | Admin
            deriving (Show,Eq,Read)
type Log = UserId -> Text
type LogWriter s = Writer [Log] s
type Transition s = (s -> LogWriter (Either Text s))

fail :: Text -> Writer [Log] (Either Text s)
fail errorMsg = W.writer ( Left errorMsg
                         , [const $  T.concat ["ERROR: ", errorMsg]] )

--------------------------------------------------------------------------------
-- Class definitions
--------------------------------------------------------------------------------

class UserC user where
  getUserId :: user -> UserId
  isUserId :: UserId -> user -> Bool
  isUserId userId user = getUserId user == userId

class StateC state where
  getCurrentPlayer :: state -> UserId
  advancePlayerOrder :: state -> state
  -- getWinner :: state -> Maybe UserId

class (StateC state, Read action, Show action) =>
      UserActionC state action where
  getTransition' :: UserId -> action -> Transition state
  isMetaAction' :: action -> Bool
  isMetaAction' = const False

data UserAction state = forall action.
                        UserActionC state action =>
                        UserAction { getActingPlayer :: UserId
                                   , getAction :: action }

does' :: UserActionC state action =>
         Proxy state -> UserId -> action -> UserAction state
does' _ = UserAction

#if false
instance forall action.
         (UserActionC state action) =>
         Read (UserAction state) where
  -- readsPrec :: Int -> ReadS a
  readsPrec _ input = [(UserAction $ read input, "")]
#endif

instance Show (UserAction state) where
  show (UserAction Admin action)      = show action
  show (UserAction (U userId) action) = userId ++ ": " ++ show action

instance Eq (UserAction s) where
  act1 == act2 = show act1 == show act2

type Game state = [UserAction state]

--------------------------------------------------------------------------------
-- play
--------------------------------------------------------------------------------

play' :: state -> Game state -> (Either Text state, [Log])
play' state as = W.runWriter $ play'' state as
  where
    getTransition :: UserAction state -> Transition state
    getTransition (UserAction userId action) = getTransition' userId action

    play'' :: state -> Game state -> LogWriter (Either Text state)
    play'' state (a:as) = do
      result <- getTransition a state
      case result of
        Right newState -> play'' newState as
        Left errorMsg  -> pure $ Left errorMsg
    play'' state []    = pure $ Right state

--------------------------------------------------------------------------------
-- debug helper functions
--------------------------------------------------------------------------------

getLog :: (a, [Log]) -> Log
getLog (_, logs) userId = T.intercalate "\n" $ map (\f -> f userId) logs

printLog :: (a, [Log]) -> UserId -> IO()
printLog gs u = putStrLn $ T.unpack $ getLog gs u

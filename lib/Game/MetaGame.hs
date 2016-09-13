{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Game.MetaGame
       ( UserId
       , UserC (..)
       , StateC (..)
       , UserActionC (..), UserAction, pack'
       , Game, play
       , getLog, printLog)
    where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W

--------------------------------------------------------------------------------
-- Game actions
--------------------------------------------------------------------------------

type UserId = String
type Log = Maybe UserId -> Text
type LogWriter s = Writer [Log] s
type Transition s = (s -> LogWriter (Either Text s))

fail :: Text -> Writer [Log] (Either Text s)
fail errorMsg = W.writer ( Left errorMsg
                         , [const $  T.concat ["ERROR: ", errorMsg]] )
class UserC user where
  getUserId :: user -> UserId

class StateC state where
  getCurrentPlayer :: state -> Maybe UserId
  getWinner :: state -> Maybe UserId

class (StateC state, Read action, Show action) => UserActionC state action where
  getTransition' :: action -> Transition state
  isMetaAction' :: action -> Bool
  isMetaAction' = const False

data UserAction state = forall action. UserActionC state action => UserAction { getAction :: action }
pack' :: UserActionC state action => Proxy state -> action -> UserAction state
pack' _ = UserAction

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
-- helper functions
--------------------------------------------------------------------------------

getTransition :: UserAction state -> Transition state
getTransition (UserAction action) = getTransition' action

--------------------------------------------------------------------------------
-- play
--------------------------------------------------------------------------------

play :: state -> Game state -> (Either Text state, [Log])
play state as = W.runWriter $ play' state as
  where
    play' :: state -> Game state -> LogWriter (Either Text state)
    play' state (a:as) = do
      result <- getTransition a state
      case result of
        Right newState -> play' newState as
        Left errorMsg  -> pure $ Left errorMsg
    play' state []    = pure $ Right state

getLog :: (a, [Log]) -> Maybe UserId -> Text
getLog (_, logs) userId = T.intercalate "\n" $ map (\f -> f userId) logs

printLog :: (a, [Log]) -> Maybe UserId -> IO()
printLog gs u = putStrLn $ T.unpack $ getLog gs u

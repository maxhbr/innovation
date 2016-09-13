{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Game.MetaGame
       ( UserId
       , IsUserable (..)
       , UserActionC (..), UserAction, pack'
       , Game, play)
    where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W

--------------------------------------------------------------------------------
-- Game actions
--------------------------------------------------------------------------------

type UserId = String
type Log = Maybe UserId -> Text
type LogWriter s = Writer [Log] s
type Transition s = (s -> Writer [Log] (Either Text s))

class IsUserable state where
  getCurrentUser :: state -> Maybe UserId

class (IsUserable state, Read action, Show action) => UserActionC state action where
  getTransition' :: action -> Transition state

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
-- helper functions, which unpack the functions of UserActionC
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

showLog :: (a, [Log]) -> Maybe UserId -> Text
showLog (_, logs) userId = T.concat $ map (\f -> f userId) logs

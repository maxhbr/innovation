{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.MetaGame.Action
       where

import           Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.Reader (ReaderT)

import           Game.MetaGame.Types.Core
import           Game.MetaGame.Types.GameState
import           Game.MetaGame.Move

--------------------------------------------------------------------------------
-- * Action

type ActionType r
  = ReaderT UserId -- ^ the user doing the action (also the logging user, ...)
            Move
            r

liftFromInner :: InnerMoveWR r -> Move r
liftFromInner = M . lift

newtype Action r
  = A { unpackAction :: ActionType r }
  deriving (Functor, Applicative, Monad)

liftFromMove :: InnerMoveWR r -> Action r
liftFromMove = A . lift . liftFromInner

takes :: UserId -> Action r -> Move r
takes uid = (flip R.runReaderT uid) . unpackAction

-- ** ActionToken
-- ActionTokens are used to identify actions

-- | an actionToken is something which
--   - has a Read and a Show instance
--   - knows its corresponding action
class (View actionToken, Eq actionToken, Read actionToken, Show actionToken) =>
      ActionToken actionToken where
  -- | returns the action corresponding to an Token
  getAction :: actionToken -> Action ()

  -- | returns, whether the board is within an state, where the turn can be applied
  stateMatchesExpectation :: actionToken -> Move Bool
  stateMatchesExpectation _ = do
    ms <- getMachineState
    return (ms == WaitForTurn)

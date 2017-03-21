{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.MetaGame.Action
       where

import Game.MetaGame.Types.Core
import Game.MetaGame.Move

import           Control.Monad.Trans.Reader (ReaderT)

--------------------------------------------------------------------------------
-- * Action

newtype Action
  = A { unpackAction :: ReaderT UserId -- ^ the user doing the action (also the logging user, ...)
                                FullMove -- ^ the move behind the action
                                () }
  deriving (Functor, Applicative, Monad)

takes :: UserId -> Action -> FullMove ()
takes uid = runActionType uid . unpackAction

-- ** ActionToken
-- ActionTokens are used to identify actions

-- | an actionToken is something which
--   - has a Read and a Show instance
--   - knows its corresponding action
class (View actionToken, Eq actionToken, Read actionToken, Show actionToken) =>
      ActionToken actionToken where
  -- | returns the action corresponding to an Token
  getAction :: actionToken -> Action

  -- | returns, whether the board is within an state, where the turn can be applied
  stateMatchesExpectation :: actionToken -> InnerMoveWR Bool
  stateMatchesExpectation _ = do
    ms <- getMachineState
    return (ms == WaitForTurn)

module Game.MetaGame.Action
       where

import Game.MetaGame.Move

--------------------------------------------------------------------------------
-- * Action

newtype Action
  = A { unpackAction :: ReaderT UserId -- ^ the user doing the action (also the logging user, ...)
                                MoveWR -- ^ the move behind the action
                                ()
      }

takes :: UserId -> Action -> MoveType ()
takes uid = runActionType uid . unpackAction

instance Monoid Action where
  mempty                = A (return mempty)
  mappend (A a1) (A a2) = A (a1 >> a2)

instance Functor Action where
  fmap f action = action >>= (return . f)

instance Applicative Action where
  pure r = A (return r)
  (A getF) <*> (A getX) = A (getF <*> getX)

instance Monad Action where
  return t    = A (return t)
  (A t) >>= f = A (t >>= (unpackAction . f))

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
  stateMatchesExpectation :: actionToken -> MoveType Bool
  stateMatchesExpectation _ = do
    ms <- getMachineState
    return (ms == WaitForTurn)

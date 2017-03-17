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

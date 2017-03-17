module Game.MetaGame.Move
       where

import           Data.Text (Text)
import           Control.Monad.Trans
import           Control.Monad.Trans.Writer (Writer, WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S

import Game.MetaGame.Types.Core
import Game.MetaGame.Types.Inquiry
import Game.MetaGame.GameState

--------------------------------------------------------------------------------
-- * Move
-- ** InnerMove

type InnerMoveType
  = ExceptT Text -- ^ uses ExceptT to communicate failures
    ( WriterT Log ) -- ^ uses WriterT to log

type InnerMoveResult r
  = ( Either Text -- ^ this maybe contains the error text
      r -- ^ this is the calculated result
    , Log ) -- ^ this contains the log

--------------------------------------------------------------------------------
-- ** Move

newtype MoveWR a r
  = M { unpackMove :: StateT GameState
                             ( InquiryLayer a )
                             r }

newtype FullMove r
  = MoveWR InnerMoveType r

liftFromInner :: InnerMoveType r -> MoveWR a r
liftFromInner = M . lift . lift . lift

type OuterMoveResult r
  = InquiryResult ( r -- ^ this is the calculated result
                  , GameState ) -- ^ this is the state of the board at the end of the calculation

type MoveResult r = InnerMoveResult (OuterMoveResult r)


-- | a 'Move' does not calculate anything, it just modifies the state (+ failures + log)

runMove :: GameState -> [Answer] -> MoveWR a -> MoveResult a
runMove gameState as = runMoveType gameState as . unpackMove

instance Monoid Move where
  mempty                = M $ S.modify id -- TODO
  mappend (M t1) (M t2) = M $ t1 >> t2

instance Functor MoveWR where
  fmap f move = move >>= (return . f)

instance Applicative MoveWR where
  pure r = M $ return r
  (M getF) <*> (M getX) = M $ do
    r <- getF
    x <- getX
    return $ r x

instance Monad MoveWR where
  return t    = M $ return t
  (M t) >>= f = M $ t >>= (unpackMove . f)


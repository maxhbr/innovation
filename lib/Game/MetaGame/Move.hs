{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.MetaGame.Move
       where

import           Data.Text (Text)
import           Data.Functor.Identity (Identity)
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
-- the persistent part, which survives single moves

type InnerMoveWRType
  = ExceptT ( Text -- ^ communicate failures
            , Int ) -- ^ number of failed command
            ( Writer Log ) -- ^ write a log

newtype InnerMoveWR r
  = IM { unpackInnerMove :: InnerMoveWRType r }
  deriving (Functor, Applicative, Monad)

type InnerMoveResult r
  = ( Either (Text, Int) -- ^ this maybe contains the error text
      r -- ^ this is the calculated result
    , Log ) -- ^ this contains the log

runInnerMoveType :: InnerMoveWR r -> InnerMoveResult r
runInnerMoveType = W.runWriter . E.runExceptT . unpackInnerMove

--------------------------------------------------------------------------------
-- ** Move

type UserIndependentMoveType r
  = StateT GameState
           InnerMoveWR
           r

newtype UserIndependentMove r
  = UIM { unpackUIMove :: UserIndependentMoveType r }
  deriving (Functor, Applicative, Monad)

type MoveType r
  = ReaderT UserId -- ^ the user doing the action (also the logging user, ...)
            UserIndependentMove
            r

newtype Move r
  = M { unpackMove :: MoveType r }
  deriving (Functor, Applicative, Monad)

liftFromInner :: InnerMoveWR r -> Move r
liftFromInner = M . lift . UIM . lift

type OuterMoveResult r
  = InquiryResult ( r -- ^ this is the calculated result
                  , GameState ) -- ^ this is the state of the board at the end of the calculation

type MoveResult r = InnerMoveResult (OuterMoveResult r)


-- | a 'Move' does not calculate anything, it just modifies the state (+ failures + log)

-- runMove :: GameState -> [Answer] -> MoveWR a -> MoveResult a
-- runMove gameState as = runMoveType gameState as . unpackMove


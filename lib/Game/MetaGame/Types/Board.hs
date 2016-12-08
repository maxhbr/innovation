{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Game.MetaGame.Types.Board
       ( BoardC (..)
       ) where

import           Data.Text (Text)
import           Control.Monad.Trans.Except (Except)

import           Game.MetaGame.Types.Core
import           Game.MetaGame.Types.GameState

class IdAble board =>
      BoardC board where
  emptyBoard :: board

  -- | the current turn is done
  advancePlayerOrder :: board -> board

  -- | atomic update
  -- this should check for winning conditions and do all other checks, which
  -- depend on the current state and could happen in mid turn
  doAtomicUpdate :: board -> Except Text board

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Game.Innovation.Rules.Rules -- TODO rename
    where

import           Prelude hiding (log)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import           Data.Proxy
import qualified Control.Lens as L

import           Game.MetaGame
import           Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import           Game.Innovation.Rules.CoreRules

module Game.Innovation.Admin
    where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Cards

data Init = Init Integer
data AddPlayer = AddPlayer UserId
data DropPlayer = DropPlayer UserId

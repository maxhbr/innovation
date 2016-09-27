{-# LANGUAGE TemplateHaskell #-}
module Game.Innovation.TypesLenses
       where

import           Control.Lens (makeLenses, Lens, Lens')
import           Game.Innovation.Types

makeLenses ''Production
makeLenses ''Productions
makeLenses ''Card
makeLenses ''Player
makeLenses ''Board

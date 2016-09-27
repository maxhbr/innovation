{-# LANGUAGE LambdaCase #-}
module Game.Innovation.CardsSpec
       where
import SpecHelper
import Data.Maybe
import Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Cards

spec :: Spec
spec =
  describe "Game.Innovation.Cards" $ do
    it "All IDemand descriptions should start with \"I demand \"" $ do
      let prefix = "I demand "
      let dogmas = concatMap _dogmas getCards
      let test = \case
            (IDemand _ desc _) -> desc `shouldStartWith` prefix
            (Dogma _ desc _)   -> desc `shouldNotContain` prefix
      mapM_ test dogmas
    it "All cards should have three productions" $ do
      let productionss = map _productions getCards
      let test = \case
            (Productions None        (Produce _) (Produce _) (Produce _)) -> True
            (Productions (Produce _) None        (Produce _) (Produce _)) -> True
            (Productions (Produce _) (Produce _) None        (Produce _)) -> True
            (Productions (Produce _) (Produce _) (Produce _) None       ) -> True
            _                                                             -> False
      all test productionss `shouldBe` True

main :: IO ()
main = hspec spec

module Game.Innovation.TypesSpec
       where
import SpecHelper
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types

spec :: Spec
spec = do
  describe "User" $ do
    it "getUserId . mkPlayer = id" $
       property $ \id -> (getUserId . mkPlayer) id == (U (id :: String))

main :: IO ()
main = hspec spec

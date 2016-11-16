module Game.Innovation.TypesSpec
       where
import SpecHelper
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types

spec :: Spec
spec = do
  describe "User" $ do
    it "idOf . mkPlayer = id" $
       property $ \id -> (idOf . mkPlayer) id == (U (id :: String))

main :: IO ()
main = hspec spec

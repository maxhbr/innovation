module Game.Innovation.RulesSpec
       where
import SpecHelper
import Game.Innovation.TestHelper
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Rules

pShouldBe :: Map Symbol Int -> Map Symbol Int -> IO ()
pShouldBe m1 m2 = mapM_ (\s -> (Map.findWithDefault 0 s m1 `shouldBe` Map.findWithDefault 0 s m2)) symbols

spec :: Spec
spec = do
  describe "productionsForStack" $ do
    it "empty stack has no production" $ do
      productionsForStack (PlayStack [] NotSplayed) `pShouldBe` (Map.fromList [])
    it  "singleton stack has productions from card" $ do
      let productions = productionsForStack (PlayStack [Card "C1" Blue Age1 (Productions None None None None) []] NotSplayed)
      productions `pShouldBe` (Map.fromList [])
      mapM_ (\splayState ->
              productionsForStack (PlayStack [Card "C1" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []] splayState)
                `pShouldBe` (Map.fromList [(Crown,1),(Tree,1),(Clock,1)])) [(minBound :: SplayState) ..]
    it  "stack splayed left productions" $
      productionsForStack (PlayStack [Card "C1" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                                     ,Card "C2" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                                     ,Card "C3" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []] SplayedLeft)
                `pShouldBe` (Map.fromList [(Crown,1),(Tree,1),(Clock,1),(Castle,2)])
    it  "stack splayed right productions" $
      productionsForStack (PlayStack [Card "C1" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                                     ,Card "C2" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                                     ,Card "C3" Blue Age1 (Productions (Produce Crown) (Produce Tree) None (Produce Castle)) []] SplayedRight)
                `pShouldBe` (Map.fromList [(Crown,3),(Tree,2),(Clock,1)])
    it  "stack splayed up productions" $
      productionsForStack (PlayStack [Card "C1" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                                     ,Card "C2" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                                     ,Card "C3" Blue Age1 (Productions (Produce Crown) (Produce Tree) None (Produce Castle)) []] SplayedUp)
                `pShouldBe` (Map.fromList [(Crown,1),(Tree,3),(Clock,1),(Castle,2)])
    it  "stack splayed up symbol counts" $ do
      let player = Player { _playerId = U "test"
                          , _playStacks = Map.fromList [(Blue
                                                        , PlayStack [Card "C1" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                                                                    ,Card "C2" Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                                                                    ,Card "C3" Blue Age1 (Productions (Produce Crown) (Produce Tree) None (Produce Castle)) []] SplayedUp)]
                          , _influence = emptyStack
                          , _dominations = Dominations []
                          , _hand = emptyStack }
      let productions = productionsOf player
      Map.lookup Crown productions `shouldBe` Just 1
      Map.lookup Tree productions `shouldBe` Just 3
      Map.lookup Clock productions `shouldBe` Just 1
      Map.lookup Castle productions `shouldBe` Just 2

main :: IO ()
main = hspec spec

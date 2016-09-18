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

spec :: Spec
spec = do
  describe "getProductionsForStack" $ do
    it "empty stack has no production" $ do
      getProductionsForStack [] NotSplayed `shouldBe` []
    it  "singleton stack has productions from card" $ do
      getProductionsForStack [Card Blue Age1 (Productions None None None None) []] NotSplayed
        `shouldBe` [None,None,None,None]
      mapM_ (\splayState ->
              getProductionsForStack [Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []] splayState
                `shouldBe` [Produce Crown,None,Produce Tree,Produce Clock]) [(minBound :: SplayState) ..]
    it  "stack splayed left productions" $
      getProductionsForStack [Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                             ,Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                             ,Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []] SplayedLeft
                `shouldBe` [Produce Crown,None,Produce Tree,Produce Clock,Produce Castle,Produce Castle]
    it  "stack splayed right productions" $
      getProductionsForStack [Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                             ,Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                             ,Card Blue Age1 (Productions (Produce Crown) (Produce Tree) None (Produce Castle)) []] SplayedRight
                `shouldBe` [Produce Crown,None,Produce Tree,Produce Clock,Produce Crown,None,Produce Crown,Produce Tree]
    it  "stack splayed up productions" $
      getProductionsForStack [Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                             ,Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                             ,Card Blue Age1 (Productions (Produce Crown) (Produce Tree) None (Produce Castle)) []] SplayedUp
                `shouldBe` [Produce Crown,None,Produce Tree,Produce Clock,None,Produce Tree,Produce Castle,Produce Tree,None,Produce Castle]
    it  "stack splayed up symbol counts" $ do
      let player = Player { _playerId = U "test"
                          , _stacks = Map.fromList [(Blue
                                                    , [Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Clock)) []
                                                      ,Card Blue Age1 (Productions (Produce Crown) None (Produce Tree) (Produce Castle)) []
                                                      ,Card Blue Age1 (Productions (Produce Crown) (Produce Tree) None (Produce Castle)) []])]
                          , _splayStates = Map.fromList [(Blue ,SplayedUp)]
                          , _influence = []
                          , _dominations = []
                          , _hand = [] }
      let symbols = getSymbols player
      Map.lookup Crown symbols `shouldBe` Just 1
      Map.lookup Tree symbols `shouldBe` Just 3
      Map.lookup Clock symbols `shouldBe` Just 1
      Map.lookup Castle symbols `shouldBe` Just 2

main :: IO ()
main = hspec spec

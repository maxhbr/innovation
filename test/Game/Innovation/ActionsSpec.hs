module Game.Innovation.ActionsSpec
       where
import SpecHelper
import Control.Lens
import Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Actions

deckId = "base" :: String -- DeckId
seed = 12345 :: Int

printLog = TIO.putStrLn . viewLog Admin . extractLog

spec :: Spec
spec =
  describe "Game.Innovation.Actions.Admin" $ do
    it "emty game setup" $ do
      let game = G ([] :: [Action State])
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      view players state `shouldBe` []
    it "just init" $ do
      let game = G [ Admin `does` Init deckId seed ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      view players state `shouldBe` []
      isJust (Map.lookup Age1 (view drawStacks state)) `shouldBe` True
    it "just init and start" $ do
      let game = G [ Admin `does` Init deckId seed
                   , Admin `does` StartGame ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` False
    it "just init + addPlayers" $ do
      let game = G [ Admin `does` Init deckId seed
                   , Admin `does` AddPlayer "user1"
                   , Admin `does` AddPlayer "user2" ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      map getId (view players state) `shouldBe` [U "user2", U "user1"]
    it "just init + addPlayers + StartGame" $ do
      let game = G [ Admin `does` Init deckId seed
                   , Admin `does` AddPlayer "user1"
                   , Admin `does` AddPlayer "user2"
                   , Admin `does` StartGame ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      map getId (view players state) `shouldBe` [U "user2", U "user1"]
      view machineState state `shouldNotBe` Prepare

main :: IO ()
main = hspec spec

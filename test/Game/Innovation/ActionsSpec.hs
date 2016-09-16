module Game.Innovation.ActionsSpec
       where
import SpecHelper
import Control.Lens
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types
import Game.Innovation.Actions

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
      let game = G [ Admin `does` Init seed ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isNothing stateM `shouldBe` True
    it "just init and start" $ do
      let game = G [ Admin `does` Init seed
                   , Admin `does` StartGame ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` False
    it "just init + addPlayers" $ do
      let game = G [ Admin `does` Init seed
                   , Admin `does` AddPlayer "user1"
                   , Admin `does` AddPlayer "user2" ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      map getUserId (view players state) `shouldBe` [U "user1", U "user2"]
    it "just init + addPlayers + StartGame" $ do
      let game = G [ Admin `does` Init seed
                   , Admin `does` AddPlayer "user1"
                   , Admin `does` AddPlayer "user2"
                   , Admin `does` StartGame]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractState playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      map getUserId (view players state) `shouldBe` [U "user1", U "user2"]

main :: IO ()
main = hspec spec

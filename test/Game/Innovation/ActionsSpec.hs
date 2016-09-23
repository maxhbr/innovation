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

seed = 12345 :: Int

printLog = TIO.putStrLn . viewLog Admin . extractLog

spec :: Spec
spec =
  describe "Game.Innovation.Actions.Admin" $ do
    it "emty game setup" $ do
      let game = G (reverse [] :: [Turn Board])
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractBoard playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      view players state `shouldBe` []
      view machineState state `shouldBe` Prepare
      let log = (viewLog Admin . extractLog) playResult
      log `shouldBe` (T.pack "")
    it "just init" $ do
      let game = G $ reverse [ Admin `does` Init ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractBoard playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      view players state `shouldBe` []
      isJust (Map.lookup Age1 (view drawStacks state)) `shouldBe` True
      view machineState state `shouldBe` Prepare
    it "just init and start" $ do
      let game = G $ reverse [ Admin `does` Init
                             , Admin `does` StartGame seed ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractBoard playResult
      isJust stateM `shouldBe` False
    it "just init and start should not recover" $ do
      let game = G $ reverse [ Admin `does` Init
                             , Admin `does` StartGame seed
                               -- the following should not appear in the log
                             , Admin `does` AddPlayer "user1"
                             , Admin `does` AddPlayer "user2"
                             , Admin `does` StartGame seed]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractBoard playResult
      isJust stateM `shouldBe` False
      let log = (viewLog Admin . extractLog) playResult
      (T.pack "user1") `T.isInfixOf` log `shouldBe` False
    it "just init + addPlayers" $ do
      let game = G $ reverse [ Admin `does` Init
                             , Admin `does` AddPlayer "user1"
                             , Admin `does` AddPlayer "user2" ]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractBoard playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      map getId (view players state) `shouldBe` [U "user2", U "user1"]
      view machineState state `shouldBe` Prepare
    it "just init + addPlayers + StartGame" $ do
      let game = G $ reverse [ Admin `does` Init
                             , Admin `does` AddPlayer "user1"
                             , Admin `does` AddPlayer "user2"
                             , Admin `does` StartGame seed]
      let playResult = play game
      printLog playResult
      extractGameResult playResult `shouldBe` NoWinner
      let stateM = extractBoard playResult
      isJust stateM `shouldBe` True
      let state = fromJust stateM
      length (view players state) `shouldBe` 2
      view machineState state `shouldNotBe` Prepare

main :: IO ()
main = hspec spec

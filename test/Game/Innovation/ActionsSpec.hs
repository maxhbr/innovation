module Game.Innovation.ActionsSpec
       where
import SpecHelper
import Control.Lens
import Data.Maybe
import Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types
import           Game.Innovation.TypesLenses
import Game.Innovation.Actions

seed = 12345 :: Int

printLog = TIO.putStrLn . viewLog Admin . extractLog


spec :: Spec
spec = let
  isSuccessfullGameWithoutWinner game = do
    let playResult = play game
    printLog playResult
    extractGameResult playResult `shouldBe` NoWinner
    let stateM = extractBoard playResult
    isJust stateM `shouldBe` True
    let state = fromJust stateM
    return (state, playResult)

  isFailedGame game = do
    let playResult = play game
    printLog playResult
    extractGameResult playResult `shouldBe` NoWinner
    let stateM = extractBoard playResult
    isJust stateM `shouldBe` False
    return playResult

  in describe "Game.Innovation.Actions.Admin" $ do

    let emptyGame = mkG []
    it "emty game setup" $ do
      (state, playResult) <- isSuccessfullGameWithoutWinner emptyGame
      view players state `shouldBe` []
      view machineState state `shouldBe` Prepare
      (viewLog Admin . extractLog) playResult `shouldBe` (T.pack "")

    let initedGame = mkG [Admin `does` Init] <> emptyGame
    it "just init" $ do
      (state, playResult) <- isSuccessfullGameWithoutWinner initedGame
      view players state `shouldBe` []
      isJust (Map.lookup Age1 (view drawStacks state)) `shouldBe` True
      view machineState state `shouldBe` Prepare

    let toEarlyStartedGame = initedGame <=> (Admin `does` StartGame seed)
    it "just init and start" $ do
      playResult <- isFailedGame toEarlyStartedGame
      let log = (viewLog Admin . extractLog) playResult
      (T.unpack log) `shouldContain` "Error"

    let toEarlyStartedGameTryingtoRecover = toEarlyStartedGame <>
                     -- the following should not appear in the log
               mkG [ Admin `does` AddPlayer "user1"
                   , Admin `does` AddPlayer "user2"
                   , Admin `does` StartGame seed ]
    it "just init and start should not recover" $ do
      playResult <- isFailedGame toEarlyStartedGameTryingtoRecover
      let log = (viewLog Admin . extractLog) playResult
      (T.pack "user1") `T.isInfixOf` log `shouldBe` False

    let gameWithPlayers = initedGame <>
                          mkG [Admin `does` AddPlayer "user1"
                              , Admin `does` AddPlayer "user2" ]
    it "just init + addPlayers" $ do
      (state, _) <- isSuccessfullGameWithoutWinner gameWithPlayers
      map getUId (view players state) `shouldBe` [U "user2", U "user1"]
      view machineState state `shouldBe` Prepare

    let startedGame = gameWithPlayers <=> (Admin `does` StartGame seed)
    it "just init + addPlayers + StartGame" $ do
      (state, _) <- isSuccessfullGameWithoutWinner startedGame
      length (view players state) `shouldBe` 2
      view machineState state `shouldNotBe` Prepare

    let someActionsTaken = startedGame <>
                           mkG [ U "user2" `does` Draw
                               , U "user1" `does` Draw]
    it "just init + addPlayers + StartGame + draw" $ do
      (state, _) <- isSuccessfullGameWithoutWinner someActionsTaken
      length (view players state) `shouldBe` 2
      view machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

    let drawManyCards = someActionsTaken <>
                        mkG [ U "user1" `does` Draw
                            , U "user2" `does` Draw
                            , U "user2" `does` Draw
                            , U "user1" `does` Draw
                            , U "user1" `does` Draw
                            , U "user2" `does` Draw
                            , U "user2" `does` Draw
                            , U "user1" `does` Draw]
    it "just init + addPlayers + StartGame + drawMany" $ do
      (state, _) <- isSuccessfullGameWithoutWinner drawManyCards
      length (view players state) `shouldBe` 2
      view machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

    let playCardNotInHand = someActionsTaken <=>
                           (U "user1" `does` Play (CardId "[Age1: XXX]"))
    it "just init + addPlayers + StartGame + draw + playStupid" $ do
      playResult <- isFailedGame playCardNotInHand
      let log = (viewLog Admin . extractLog) playResult
      (T.unpack log) `shouldContain` "Error"

    let playValidCard = someActionsTaken <=>
                        (U "user1" `does` Play (CardId "[Age1: Sailing]"))
    it "just init + addPlayers + StartGame + draw + play" $ do
      (state, _) <- isSuccessfullGameWithoutWinner playValidCard
      length (view players state) `shouldBe` 2
      view machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

main :: IO ()
main = hspec spec

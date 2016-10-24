module Game.Innovation.ActionsSpec
       where
import SpecHelper
import qualified Control.Lens as L
import Data.Maybe
import Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Game.Innovation.TestHelper

import Game.MetaGame
import Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import Game.Innovation.ActionTokens

seed = 12345 :: Int

printLog = TIO.putStrLn . viewLog Admin . extractLog


spec :: Spec
spec = let
  isSuccessfullGameWithoutWinner game = do
    let playResult = play game
    printLog playResult

    extractWinner playResult `shouldBe` Nothing

    let stateM = extractBoard playResult
    isJust stateM `shouldBe` True

    -- history should only be passed through
    let resultGame = extractGame playResult
    show resultGame `shouldBe` show game

    let state = fromJust stateM
    return (state, playResult)

  isFailedGame game = do
    let playResult = play game
    printLog playResult

    extractWinner playResult `shouldBe` Nothing
    let stateM = extractBoard playResult
    isJust stateM `shouldBe` False

    -- history should be trimmed to a prefix (in fact the longest valid one)
    let resultGame = extractGame playResult
    show game `shouldNotBe` show resultGame

    return playResult

  isGameWithOutstandingQuestions game = do
    let playResult = play game
    printLog playResult

    extractWinner playResult `shouldBe` Nothing

    let stateM = extractBoard playResult
    isJust stateM `shouldBe` True

    undefined -- TODO: check outstanding questions

    return playResult

  in describe "Game.Innovation.Actions.Admin" $ do

    let emptyGame = mkG []
    it "emty game setup" $ do
      (state, playResult) <- isSuccessfullGameWithoutWinner emptyGame
      L.view L.players state `shouldBe` []
      L.view L.machineState state `shouldBe` Prepare
      (viewLog Admin . extractLog) playResult `shouldBe` (T.pack "")

    let toEarlyStartedGame = emptyGame <=> (Admin `does` StartGame seed)
    it "just start" $ do
      playResult <- isFailedGame toEarlyStartedGame
      let log = (viewLog Admin . extractLog) playResult
      T.unpack log `shouldContain` "Error"

    let toEarlyStartedGameTryingtoRecover = toEarlyStartedGame <>
                     -- the following should not appear in the log
               mkG [ Admin `does` AddPlayer "user1"
                   , Admin `does` AddPlayer "user2"
                   , Admin `does` StartGame seed ]
    it "just start should not recover" $ do
      playResult <- isFailedGame toEarlyStartedGameTryingtoRecover
      let log = (viewLog Admin . extractLog) playResult
      T.pack "user1" `T.isInfixOf` log `shouldBe` False

    let gameWithPlayers = emptyGame <>
                          mkG [ Admin `does` AddPlayer "user1"
                              , Admin `does` AddPlayer "user2" ]
    it "just addPlayers" $ do
      (state, _) <- isSuccessfullGameWithoutWinner gameWithPlayers
      map getUId (L.view L.players state) `shouldBe` [U "user2", U "user1"]
      L.view L.machineState state `shouldBe` Prepare

    let startedGame = gameWithPlayers <=> (Admin `does` StartGame seed)
    it "just addPlayers + StartGame" $ do
      (state, _) <- isSuccessfullGameWithoutWinner startedGame
      length (L.view L.players state) `shouldBe` 2
      L.view L.machineState state `shouldNotBe` Prepare

    let startedGameCooseOneInitalCard = startedGame
                                     <=> (U "user2" `chooses` (`Answer` [0]))
    -- it "just addPlayers + StartGame + choose1" $ do
    --   (state, _) <- isGameWithOutstandingQuestions startedGameCooseOneInitalCard
    --   length (L.view L.players state) `shouldBe` 2
    --   L.view L.machineState state `shouldNotBe` Prepare
    --   exactlyAllCardsArePresent state `shouldBe` True

    let startedGameCooseOtherInitalCard = startedGameCooseOneInitalCard
                                     <=> (U "user1" `chooses` (`Answer` [1]))
    it "just addPlayers + StartGame + choose2" $ do
      (state, _) <- isSuccessfullGameWithoutWinner startedGameCooseOtherInitalCard

      length (L.view L.players state) `shouldBe` 2
      L.view L.machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

    let someActionsTaken = startedGameCooseOtherInitalCard <>
                           mkG [ U "user1" `does` Draw
                               , U "user2" `does` Draw]
    it "just addPlayers + StartGame + draw" $ do
      (state, _) <- isSuccessfullGameWithoutWinner someActionsTaken
      length (L.view L.players state) `shouldBe` 2
      L.view L.machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

    let drawManyCards = someActionsTaken <>
                        mkG [ U "user2" `does` Draw
                            , U "user1" `does` Draw
                            , U "user1" `does` Draw
                            , U "user2" `does` Draw
                            , U "user2" `does` Draw
                            , U "user1" `does` Draw
                            , U "user1" `does` Draw
                            , U "user2" `does` Draw]
    it "just addPlayers + StartGame + drawMany" $ do
      (state, _) <- isSuccessfullGameWithoutWinner drawManyCards
      length (L.view L.players state) `shouldBe` 2
      L.view L.machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

    let playCardNotInHand = someActionsTaken <=>
                           (U "user2" `does` Play (CardId "[Age1: XXX]"))
    it "just addPlayers + StartGame + draw + playStupid" $ do
      playResult <- isFailedGame playCardNotInHand
      let log = (viewLog Admin . extractLog) playResult
      T.unpack log `shouldContain` "Error"

    let playValidCard = someActionsTaken <=>
                        (U "user2" `does` Play (CardId "[Age10: Databases]"))
    it "just addPlayers + StartGame + draw + play" $ do
      (state, _) <- isSuccessfullGameWithoutWinner playValidCard
      length (L.view L.players state) `shouldBe` 2
      L.view L.machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

    let activateValidCard = playValidCard <>
                            mkG [ U "user1" `does` Draw
                                , U "user1" `does` Draw
                                , U "user2" `does` Activate Green]
    it "just addPlayers + StartGame + draw + play + activate" $ do
      (state, _) <- isSuccessfullGameWithoutWinner activateValidCard
      length (L.view L.players state) `shouldBe` 2
      L.view L.machineState state `shouldNotBe` Prepare
      exactlyAllCardsArePresent state `shouldBe` True

main :: IO ()
main = hspec spec

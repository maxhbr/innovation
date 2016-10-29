{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Monad
import Game.Innovation.TestHelper
import Test.Hspec.Core.Spec
import qualified System.HsTColors as HsT
-- import Test.Hspec.Core.Spec.Monad

import Game.MetaGame
import Game.Innovation.Types
import qualified Game.Innovation.TypesLenses as L
import Game.Innovation.ActionTokens

data TestGameDesc
  = TGD { _isFailed :: Bool
        , _logContians :: Maybe String
        , _expectedState :: MachineState -> IO ()
        , _asserts :: PlayResult Board -> IO () }

-- ** Generators
emptyTGD = TGD False Nothing (const (pure ())) (const (pure ()))
errorTGD = emptyTGD { _isFailed = True
                    , _logContians = Just "Error" }

-- ** Modifiers
withoutWinner tgd = tgd {_asserts=(\playResult -> extractWinner playResult `shouldBe` Nothing)}
prepareTGD tgd = withoutWinner $ tgd { _expectedState = (`shouldBe` Prepare) }
waitingForTurnTGD tgd = withoutWinner $ tgd { _expectedState = (`shouldBe` WaitForTurn) }
waitingForChoiceTGD tgd = withoutWinner $ tgd { _expectedState = \case
                                   WaitForChoice _ -> pure ()
                                   _               -> expectationFailure "expected: WaitForChoice" }
byAdmin tgd = tgd {_asserts=
                      (\playResult -> (do
                                          _asserts tgd playResult
                                          (getCurrentPlayer' . fromJust . extractBoard) playResult `shouldBe` Admin))}
byUser1 tgd = tgd {_asserts=
                      (\playResult -> (do
                                          _asserts tgd playResult
                                          (getCurrentPlayer' . fromJust . extractBoard) playResult `shouldBe` U "user1"))}
byUser2 tgd = tgd {_asserts=
                      (\playResult -> (do
                                          _asserts tgd playResult
                                          (getCurrentPlayer' . fromJust . extractBoard) playResult `shouldBe` U "user2"))}

data TestGameStepDefn
  = TGSD { _desc :: String
         , _transition :: Game Board -> Game Board
         , _stateDesc :: TestGameDesc
         , _persistentAsserts :: PlayResult Board -> IO ()
         }
emptyTGSD = TGSD "" id emptyTGD (const (pure ()))

type TestGameDefn = [TestGameStepDefn]

runTestGameStep :: Game Board -> TestGameStepDefn ->  SpecM (Arg (IO ())) (Game Board)
runTestGameStep game tgsd = do
  let newGame = (_transition tgsd) game
  let tgd = _stateDesc tgsd
  it (_desc tgsd ++ (if _isFailed tgd
                     then " should fail"
                     else "")) $ do
    let playResult = play newGame
    let stateM = extractBoard playResult
    when ((not . _isFailed) tgd)
      (isJust stateM `shouldBe` True)

    -- print log
    (TIO.putStrLn . viewLog (if ((not . _isFailed) tgd)
                             then ((getCurrentPlayer' . fromJust) stateM)
                             else Admin) . extractLog) playResult

    _asserts tgd playResult
    _persistentAsserts tgsd playResult

    -- check whether log contains potentially given string
    when (isJust ((_logContians . _stateDesc) tgsd))
      (do
          let log = (viewLog Admin . extractLog) playResult
          (HsT.uncolor . T.unpack) log `shouldContain` (fromJust . _logContians) tgd)

    if ((not . _isFailed) tgd)
      then (do
               -- -- history should only be passed through
               -- let resultGame = extractGame playResult
               -- show resultGame `shouldBe` show newGame

               (_expectedState tgd) ((getMachineState' . fromJust) stateM))
      else (isJust stateM `shouldBe` False)

  return (if (_isFailed tgd)
          then game
          else newGame)

runTestGame :: Game Board -> TestGameDefn -> Spec
runTestGame _ [] = pure ()
runTestGame game (tg:tgs) = do
  newGame <- runTestGameStep game tg
  runTestGame newGame (map (\tg' -> tg' { _persistentAsserts=_persistentAsserts tg >> _persistentAsserts tg'}) tgs)

seed = 1234 :: Int

testGame :: TestGameDefn
testGame = [ emptyTGSD{ _desc = "empty game"
                      , _stateDesc = (prepareTGD . byAdmin) emptyTGD
                      }
           , emptyTGSD{ _desc = "just start empty game"
                      , _transition = (<=> (Admin `does` StartGame seed))
                      , _stateDesc = errorTGD
                      }
           , emptyTGSD{ _desc = "just start should not recover"
                      , _transition = (<> mkG [ Admin `does` StartGame seed
                                              , Admin `does` AddPlayer "user1"
                                              , Admin `does` AddPlayer "user2"
                                              , Admin `does` StartGame seed ])
                      , _stateDesc = errorTGD
                      }
           , emptyTGSD{ _desc = "Just add players"
                      , _stateDesc = (prepareTGD . byAdmin) emptyTGD
                      , _transition = (<> mkG [ Admin `does` AddPlayer "user1"
                                              , Admin `does` AddPlayer "user2" ])
                      }
           , emptyTGSD{ _desc = "Start"
                      , _stateDesc = (waitingForChoiceTGD . byUser1) emptyTGD
                      , _transition = (<=> (Admin `does` StartGame seed))
                      }
           , emptyTGSD{ _desc = "Start again"
                      , _stateDesc = errorTGD
                      , _transition = (<=> (Admin `does` StartGame seed))
                      }
           -- , emptyTGSD{ _desc = "stupid choice of first player"
           --            , _stateDesc = (waitingForChoiceTGD . byUser1) emptyTGD
           --            , _transition = (<=> (U "user2" `chooses` (`Answer` [999])))
           --            }
           , emptyTGSD{ _desc = "first player should be able to correct answer"
                      , _stateDesc = (waitingForChoiceTGD . byUser1) emptyTGD
                      , _transition = (<=> (U "user2" `chooses` (`Answer` [0])))
                      }
           , emptyTGSD{ _desc = "second player should be able to choose"
                      , _stateDesc = (waitingForTurnTGD . byUser1) emptyTGD
                      , _transition = (<=> (U "user1" `chooses` (`Answer` [1])))
                      }
           , emptyTGSD{ _desc = "Just draw twice"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = ( <> mkG [ U "user1" `does` Draw
                                               , U "user2" `does` Draw])
                      }
           , emptyTGSD{ _desc = "put into play stupid"
                      , _stateDesc = errorTGD
                      , _transition = (<=> (U "user2" `does` Play (CardId "[Age1: XXX]")))
                      }
           , emptyTGSD{ _desc = "put into play"
                      , _stateDesc = (waitingForTurnTGD . byUser1) emptyTGD
                      , _transition = (<=> (U "user2" `does` Play (CardId "[Age1: The Wheel]")))
                      }
           , emptyTGSD{ _desc = "activateStupid"
                      , _stateDesc = errorTGD
                      , _transition = (<> mkG [ U "user1" `does` Draw
                                              , U "user1" `does` Draw
                                              , U "user2" `does` Activate Red])
                      }
           , emptyTGSD{ _desc = "activate"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = (<> mkG [ U "user1" `does` Draw
                                              , U "user1" `does` Draw
                                              , U "user2" `does` Activate Green])
                      }
           , emptyTGSD{ _desc = "draw many"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = (<> mkG [ U "user2" `does` Draw
                                              , U "user1" `does` Draw
                                              , U "user1" `does` Draw
                                              , U "user2" `does` Draw
                                              , U "user2" `does` Draw
                                              , U "user1" `does` Draw
                                              , U "user1" `does` Draw
                                              , U "user2" `does` Draw])
                      }
           ]

spec = describe "Game.Innovation.Actions" $ do
  runTestGame (mkG []) testGame

main :: IO ()
main = hspec spec

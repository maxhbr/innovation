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

import Game.Innovation.Types
import Game.Innovation.Types.InnovationState (idForUIds)
import qualified Game.Innovation.TypesLenses as L
import Game.Innovation.ActionTokens

data TestGameDesc
  = TGD { _isFailed :: Bool
        , _logContians :: Maybe String
        , _expectedState :: MachineState -> IO ()
        , _asserts :: PlayResult -> IO () }

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
                                          extractCurrentPlayer playResult `shouldBe` Admin))}
byUser1 tgd = tgd {_asserts=
                      (\playResult -> (do
                                          _asserts tgd playResult
                                          extractCurrentPlayer playResult `shouldBe` user1))}
byUser2 tgd = tgd {_asserts=
                      (\playResult -> (do
                                          _asserts tgd playResult
                                          extractCurrentPlayer playResult `shouldBe` user2))}

data TestGameStepDefn
  = TGSD { _desc :: String
         , _transition :: Game -> Game
         , _stateDesc :: TestGameDesc
         , _persistentAsserts :: PlayResult -> IO ()
         }
emptyTGSD = TGSD "" id emptyTGD (const (pure ()))
errorTGSD = TGSD "" id errorTGD (const (pure ()))

type TestGameDefn = [TestGameStepDefn]

runTestGameStep :: Game -> TestGameStepDefn ->  SpecM (Arg (IO ())) Game
runTestGameStep game tgsd = do
  let newGame = (_transition tgsd) game
  let tgd = _stateDesc tgsd
  it (_desc tgsd ++ (if _isFailed tgd
                     then " should fail"
                     else "")) $ do
    let playResult = play _ newGame
    let stateM = extractGameState playResult

    -- print log
    (TIO.putStrLn . viewLog (if ((not . _isFailed) tgd)
                             then ((fromJust . extractWinner) playResult)
                             else Admin) . extractLog) playResult

    unless (_isFailed tgd)
      (isJust stateM `shouldBe` True)

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

               (_expectedState tgd) ((fmap setMachineState) stateM))
      else (isJust stateM `shouldBe` False)

  return (if (_isFailed tgd)
          then game
          else newGame)

runTestGame :: Game -> TestGameDefn -> Spec
runTestGame _ [] = pure ()
runTestGame game (tg:tgs) = do
  newGame <- runTestGameStep game tg
  runTestGame newGame (map (\tg' -> tg' { _persistentAsserts=do
                                          _persistentAsserts tg
                                          _persistentAsserts tg'}) tgs)

seed = 1234 :: Int
user1name = "user1":: String
user2name = "user2":: String
user1 = U user1name :: UserId
user2 = U user2name :: UserId

testGame :: TestGameDefn
testGame = [ emptyTGSD{ _desc = "empty game"
                      , _stateDesc = (prepareTGD . byAdmin) emptyTGD
                      }
           , errorTGSD{ _desc = "just start empty game"
                      , _transition = (<=> (Admin `does` StartGame seed))
                      }
           , errorTGSD{ _desc = "just start should not recover"
                      , _transition = (<> mkG [ Admin `does` StartGame seed
                                              , Admin `does` AddPlayer user1name
                                              , Admin `does` AddPlayer user2name
                                              , Admin `does` StartGame seed ])
                      }
           , emptyTGSD{ _desc = "Just add players"
                      , _stateDesc = (prepareTGD . byAdmin) emptyTGD
                      , _transition = (<> mkG [ Admin `does` AddPlayer user1name
                                              , Admin `does` AddPlayer user2name ])
                      , _persistentAsserts = \playResult -> ((fmap (getIdFyObject idForUIds)) . extractGameState) playResult `shouldBe` (Just [user2, user1])
                      }
           , emptyTGSD{ _desc = "Start"
                      , _stateDesc = (waitingForChoiceTGD . byUser1) emptyTGD
                      , _transition = (<=> (Admin `does` StartGame seed))
                      }
           , errorTGSD{ _desc = "Start again"
                      , _transition = (<=> (Admin `does` StartGame seed))
                      }
           -- , emptyTGSD{ _desc = "stupid choice of first player"
           --            , _stateDesc = (waitingForChoiceTGD . byUser1) emptyTGD
           --            , _transition = (<=> (user2 `chooses` (`Answer` [999])))
           --            }
           , emptyTGSD{ _desc = "first player should be able to correct answer"
                      , _stateDesc = (waitingForChoiceTGD . byUser1) emptyTGD
                      , _transition = (<=> (user2 `chooses` (`Answer` [0])))
                      }
           , emptyTGSD{ _desc = "second player should be able to choose"
                      , _stateDesc = (waitingForTurnTGD . byUser1) emptyTGD
                      , _transition = (<=> (user1 `chooses` (`Answer` [1])))
                      }
           , emptyTGSD{ _desc = "Just draw twice"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = ( <> mkG [ user2 `does` Draw
                                               , user1 `does` Draw])
                      }
           , errorTGSD{ _desc = "put into play stupid"
                      , _transition = (<=> (user1 `does` Play (CardId "[Age1: XXX]")))
                      }
           , errorTGSD{ _desc = "dominate without influence"
                      , _transition = (<=> (user1 `does` Dominate Age1))
                      }
           , emptyTGSD{ _desc = "put into play"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = (<=> (user1 `does` Play (CardId "[Age1: The Wheel]")))
                      }
           , errorTGSD{ _desc = "activate stupid"
                      , _transition = (<> mkG [ user2 `does` Draw
                                              , user2 `does` Draw
                                              , user1 `does` Activate Purple])
                      }
           , emptyTGSD{ _desc = "activate"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = (<> mkG [ user2 `does` Draw
                                              , user2 `does` Draw
                                              , user1 `does` Activate Green])
                      }
           , emptyTGSD{ _desc = "start scoring"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = (<> mkG [ user1 `does` Draw
                                              , user2 `does` Activate Green
                                              , user1 `chooses` (`Answer` [0])
                                              , user2 `does` Activate Green
                                              , user1 `chooses` (`Answer` [0])
                                              , user1 `does` Draw
                                              ])
                      }
           , emptyTGSD{ _desc = "draw many"
                      , _stateDesc = (waitingForTurnTGD . byUser2) emptyTGD
                      , _transition = (<> mkG [ user1 `does` Draw
                                              , user2 `does` Draw
                                              , user2 `does` Draw
                                              , user1 `does` Draw
                                              , user1 `does` Draw
                                              , user2 `does` Draw
                                              , user2 `does` Draw
                                              , user1 `does` Draw])
                      }
           ]

spec = describe "Game.Innovation.Actions" $ do
  runTestGame (mkG []) testGame

main :: IO ()
main = hspec spec

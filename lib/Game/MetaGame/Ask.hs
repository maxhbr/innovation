{-# LANGUAGE LambdaCase #-}
module Game.MetaGame.Ask
       where

import           Data.List (nub)
import           Data.Monoid
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Lazy as S

import           Game.MetaGame.Types
import           Game.MetaGame.Helper

--------------------------------------------------------------------------------
-- * helper to define inquiry restrictions

emptyInq :: String -> UserId -> Inquiry a
emptyInq question uid = Inquiry uid question [] mempty

uniqueRestr :: InqRestr
uniqueRestr = InqRestr $ \cs -> cs == nub cs

numMaxRestr :: Int -> InqRestr
numMaxRestr n = InqRestr $ \cs -> length cs <= n

numEqRestr :: Int -> InqRestr
numEqRestr n = InqRestr $ \cs -> length cs == n

--------------------------------------------------------------------------------
-- * The method to invoke an Inquiry

ask :: Show a =>
       Inquiry a -> MoveType [a]
ask inq = case inquiryOptions inq of
  [] ->  do
    logInfo "empty inquiry"
    return []
  _  -> do
    answers <- lift S.get
    case answers of
      []     -> do
        setMachineState (WaitForChoice inq)
        S.get >>= (lift . lift . E.throwE)
      (a:as) -> do
        (lift . S.put) as
        if a `matchesInquiry` inq
          then return (extractAnswers inq a)
          else do
          logWarn "answer does not match inquiry, ask again"
          ask inq

matchesInquiry :: Answer -> Inquiry a -> Bool
matchesInquiry (Answer cp cs) (Inquiry ap _ opts (InqRestr restr)) = playerMatches
                                                                     && allAreInRange
                                                                     && restr cs
  where
    playerMatches = cp == ap
    allAreInRange = let
      numOfOpts = length opts
      in all (\i -> 0 <= i && i < numOfOpts) cs

extractAnswers :: Inquiry a -> Answer -> [a]
extractAnswers (Inquiry _ _ ios _) (Answer _ as) = map (\i -> ios !! i) as

--------------------------------------------------------------------------------
-- * Helper to ask things

askForBool :: UserId -> String -> MoveType Bool
askForBool uid q = fmap (\case
                            [True] -> True
                            _      -> False)
                        (ask $ Inquiry uid q [True] (numMaxRestr 1))

chooseOneOf :: Show a =>
               UserId -> String -> [a] -> MoveType [a]
chooseOneOf = chooseNOf 1

chooseNOf :: Show a =>
             Int -> UserId -> String -> [a] -> MoveType [a]
chooseNOf n uid q aws = ask $ Inquiry uid ("Choose " ++ show n ++ " of " ++ q) aws (uniqueRestr <> numEqRestr n)
-- chooseManyOf :: UserId -> _ -> MoveType board [a]

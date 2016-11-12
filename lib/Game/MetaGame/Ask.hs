{-# LANGUAGE LambdaCase #-}
module Game.MetaGame.Ask
       where

import           Data.List (nub)
import           Data.Monoid
import           Data.Proxy
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

ask :: (BoardC board, Show a) =>
       Inquiry a -> MoveType board [a]
ask inq = case inquiryOptions inq of
  [] ->  do
    logInfo "empty inquiry"
    return []
  _  -> do
    answers <- lift S.get
    case answers of
      []     -> do
        b <- S.gets (setMachineState' (WaitForChoice inq))
        (lift . lift . E.throwE) b
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

askForBool :: BoardC board =>
              Proxy board -> UserId -> String -> MoveType board Bool
askForBool _ uid q = fmap (\case
                            [True] -> True
                            _      -> False)
                        (ask $ Inquiry uid q [True] (numMaxRestr 1))

chooseOneOf :: (BoardC board, Show a) =>
               Proxy board -> UserId -> String -> [a] -> MoveType board [a]
chooseOneOf proxy = chooseNOf proxy 1

chooseNOf :: (BoardC board, Show a) =>
             Proxy board -> Int -> UserId -> String -> [a] -> MoveType board [a]
chooseNOf _ n uid q aws = ask $ Inquiry uid ("Choose " ++ show n ++ " of " ++ q) aws (uniqueRestr <> numEqRestr n)
-- chooseManyOf :: UserId -> _ -> MoveType board [a]

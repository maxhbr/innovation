module Game.MetaGame.Ask
       where

import           Data.List (nub)
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State.Lazy (State, StateT)
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Lens as L

import           Game.MetaGame.Types
import           Game.MetaGame.Helper

matchesInquiry :: Answer -> Inquiry a -> Bool
matchesInquiry (Answer cp cs) (Inquiry ap _ opts restr) = playerMatches
                                                       && noneAreDuplicates
                                                       && allAreInRange
                                                       && restr cs
  where
    playerMatches = cp == ap
    noneAreDuplicates = nub cs == cs -- ^ might be a problem for some. Maybe this should be encapsulated in 'restr'
    allAreInRange = let
      numOfOpts = length opts
      in all (\i -> 0 <= i && i < numOfOpts) cs

extractAnswers :: Inquiry a -> Answer -> [a]
extractAnswers (Inquiry _ _ ios _) (Answer _ as) = map (\i -> ios !! i) as

ask :: BoardC board =>
       Inquiry a -> MoveType board [a]
ask inq = do
  answers <- lift S.get
  case answers of
    []     -> do
      -- TODO: except or break out?
      undefined
    (a:as) -> do
      unless (a `matchesInquiry` inq) $
        logError "answer does not match inquiry"
      (lift . S.put) as
      return $ extractAnswers inq a

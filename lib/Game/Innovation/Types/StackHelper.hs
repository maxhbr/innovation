module Game.Innovation.Types.StackHelper
       where

import qualified Data.List as List
import qualified Control.Arrow as Arr

import           Game.Innovation.Types.Core

pushCards :: Stack a =>
             [Card] -> a -> a
pushCards cs = onRawStack (cs ++)

pushBottomCards :: Stack a =>
                   [Card] -> a -> a
pushBottomCards cs = onRawStack (++ cs)

popCards :: Stack a =>
            Int -> a -> ([Card], a)
popCards n a = ((\(cs, rs) -> (cs, setRawStack a rs)) . popCards' n . getRawStack) a
  where
    popCards' :: Int -> RawStack -> ([Card], RawStack)
    popCards' 0 rs     = ([], rs)
    popCards' _ []     = ([], [])
    popCards' n (r:rs) = (\(cs',rs') -> (r: cs', rs')) (popCards' (n-1) rs)

popCardsWith :: Stack a =>
                Int -> (Card -> Bool) -> a -> ([Card],a)
popCardsWith n p a = (Arr.second (setRawStack a)) (popCardsWith' n p (getRawStack a))
  where
    popCardsWith' 0 _ a      = ([], a)
    popCardsWith' _ _ []     = ([], [])
    popCardsWith' n b (r:rs) | b r       = (\(cs',rs') -> (r: cs', rs')) (popCardsWith' (n-1) b rs)
                             | otherwise = (\(cs',rs') -> (cs', r: rs')) (popCardsWith' (n-1) b rs)

popTheCard :: Stack a =>
              CardId -> a -> (Maybe Card, a)
popTheCard cid a = let
  (rs1,rs2) = List.partition (\c -> idOf c == cid) (getRawStack a) -- TODO: respects order??
  in (Arr.second (setRawStack a)) (case rs1 of
                                      [c] -> (Just c, rs2)
                                      []  -> (Nothing, rs2)
                                      _   -> undefined -- TODO: should not be reacheable
                                  )

popCard :: Stack a =>
           a -> ([Card], a)
popCard = popCards 1

pushCard :: Stack a =>
            Card -> a -> a
pushCard c = pushCards [c]

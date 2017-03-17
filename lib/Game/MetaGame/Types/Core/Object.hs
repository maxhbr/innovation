{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.MetaGame.Types.Core.Object
       where
import           Data.Typeable (Typeable, cast, typeOf)
import           Data.Foldable
--------------------------------------------------------------------------------
-- * IdAble and IdF
type family IdF a
class (Show (IdF a), Eq (IdF a), Typeable a) =>
      IdAble a where
  idOf :: a -> IdF a

  hasId :: a -> IdF a -> Bool
  hasId a idA = idOf a == idA
  checkId :: a -> IdF a -> Maybe a
  checkId a idA | a `hasId` idA = Just a
                | otherwise     = Nothing
  hasEqualId :: a -> a -> Bool
  hasEqualId a1 a2 = a1 `hasId` idOf a2

--------------------------------------------------------------------------------
-- * instances
type instance IdF (Maybe a) = Maybe (IdF a)
instance (IdAble a) =>
         IdAble (Maybe a) where
  idOf (Just a) = Just (idOf a)
  idOf Nothing  = Nothing

type instance IdF (a,b) = (IdF a, IdF b)
instance (IdAble a, IdAble b) =>
         IdAble (a,b) where
  idOf (a,b) = (idOf a, idOf b)

--------------------------------------------------------------------------------
-- * Object and World
data Object = forall object.
              (IdAble object, Typeable object) =>
              Object object
packObject :: (IdAble a, Typeable a) =>
              a -> Object
packObject = Object
unpackObject :: Typeable a =>
                Object -> Maybe a
unpackObject (Object a) = cast a
instance Show Object where
  show (Object a) = (show . idOf) a
instance Eq Object where
  (Object a1) == (Object a2) = typeOf a1 == typeOf a2
                               && test
     where
       test = case cast a2 of
         Just ca2 -> a1 `hasEqualId` ca2
         _        -> False

newtype World = World [Object]

getObject :: IdAble a =>
             IdF a -> World -> Maybe a
getObject idA (World os) = getObject' idA os
  where
    getObject' :: IdAble a =>
                 IdF a -> [Object] -> Maybe a
    getObject' idA = asum . map works
      where
        works = (>>= (`checkId` idA)) . unpackObject

setObject :: IdAble a =>
                a -> World -> World
setObject a (World os) = World (Object a : os)

modifyObject :: IdAble a =>
                (a -> a) -> IdF a -> World -> World
modifyObject f idA (World os) = World (map modifyMatching os)
  where
    modifyMatching x = case works x of
      Just y  -> (packObject . f) y
      Nothing -> x
    works = (>>= (`checkId` idA)) . unpackObject


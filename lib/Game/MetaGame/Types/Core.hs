{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Game.MetaGame.Types.Core
       ( IdF, IdAble (..)
       , IdFy (..), packIdFy, unpackIdFy
       , Object (..), World (..), getObject, setObject, modifyObject
       , getIdFyObject, setIdFyObject, modifyIdFyObject
       , UserId (..), mkUserId, isAdmin
       , LogEntry (..), (<<>), (<>>)
       , Log, viewLog, prefixLE, logAnEntryI, loggsAnEntryI, logI, loggsI, logggsI, alogI
       , chownLE
       , View (..)
       ) where

import           Prelude hiding (log)
import           Data.String
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy ()
import           Data.Typeable (Typeable, cast, typeOf)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import qualified System.HsTColors as HsT

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

type instance IdF String = String
instance IdAble String where
  idOf = id

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
-- ** Wrap to add id functionallity
data IdFy a = IdFy String a
instance Eq (IdFy a) where
  (IdFy k1 _) == (IdFy k2 _) = k1 == k2

packIdFy :: String -> a -> IdFy a
packIdFy k a = IdFy k a
unpackIdFy :: IdFy a -> a
unpackIdFy (IdFy _ a) = a

type instance IdF (IdFy a) = IdF String
instance (Typeable a) =>
         IdAble (IdFy a) where
  idOf (IdFy k _) = idOf k

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
                               && a1 `hasEqualId` (fromJust . cast) a2

data World = World [Object]

getObject :: IdAble a =>
             IdF a -> World -> Maybe a
getObject idA (World os) = getObject' idA os
  where
    getObject' :: IdAble a =>
                 IdF a -> [Object] -> Maybe a
    getObject' idA' = asum . map works
      where
        works = (>>= (`checkId` idA')) . unpackObject

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

--------------------------------------------------------------------------------
-- ** functions for generically wrapped objects
getIdFyObject :: (Typeable a) =>
                 String -> World -> Maybe a
getIdFyObject k w = fmap unpackIdFy $ getObject k w

setIdFyObject :: (Typeable a) =>
                 String -> a -> World -> World
setIdFyObject k a w = setObject (packIdFy k a) w

modifyIdFyObject :: (Typeable a) =>
                    (a -> a) -> String -> World -> World
modifyIdFyObject f = modifyObject (\(IdFy k a) -> packIdFy k (f a))

--------------------------------------------------------------------------------
-- * Users and user-related stuff

-- | should remove:
--   - newlines,
--   - whitespace at all,
-- and keep
--   - only alphanumeric + - _ , ...
sanitizeUserId :: String -> String
sanitizeUserId = id -- TODO

data UserId
  = U String -- ^ regular user, determined by its username
  | Admin -- ^ administrative user (is not allowed to play)
  | Guest -- ^ unauthorized user
  deriving (Show,Eq,Read)
mkUserId :: String -> UserId
mkUserId = U . sanitizeUserId

-- | Guest < U * < Admin
instance Ord UserId where
  compare Admin  Admin  = EQ
  compare Admin  (U _)  = GT
  compare (U _)  Admin  = LT
  compare (U u1) (U u2) = compare u1 u2
  compare Guest  Guest  = EQ
  compare Guest  _      = LT
  compare _      Guest  = GT

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

getCommonUID :: UserId -> UserId -> UserId
getCommonUID Guest uid = uid
getCommonUID uid Guest = uid
getCommonUID Admin _   = Admin
getCommonUID _   Admin = Admin
getCommonUID uid1 uid2 | uid1 == uid2 = uid1
                       | otherwise    = Admin

isAuthorizationLevel :: UserId -- ^ the asking user
                     -> UserId -- ^ the user who is to be matched
                     -> Bool
isAuthorizationLevel asker level = (asker `getCommonUID` level) == asker

--------------------------------------------------------------------------------
-- * Log

data LogLevel
  = INFO
  | ERROR
  | Fatal
  | NONE
  deriving (Eq,Show,Enum,Ord,Bounded)

-- | a logentry will be a line of a log
data LogEntry
  -- | a log entry, which is visible for all users
  = CLogE Text -- ^ the text visible for all
  -- | a log entry, which also contains content which is only visible for the admin
  | ALogE Text -- ^ the text visible for all
          Text -- ^ the restricted text
  -- | a log entry containing hidden content, visible for specific users (and admin)
  | ULogE UserId
          Text -- ^ the text visible for all
          Text -- ^ the restricted text

canonifyLE :: LogEntry -> LogEntry
canonifyLE    (ULogE Admin t1 t2) = canonifyLE (ALogE t1 t2)
canonifyLE    (ULogE Guest _  t2) = CLogE t2
canonifyLE le@(ULogE _  t1 t2)    | t1 == t2  = CLogE t1
                                  | otherwise = le
canonifyLE le@(ALogE t1 t2)       | t1 == t2  = CLogE t1
                                  | otherwise = le
canonifyLE le@(CLogE _)           = le

generifyLE :: LogEntry -> LogEntry
generifyLE (ALogE t1 t2) = ULogE Admin t1 t2
generifyLE (CLogE t1)    = ULogE Guest t1 t1
generifyLE le            = le

getRestricted, getUnrestricted :: LogEntry -> Text
getRestricted (CLogE t)     = t
getRestricted (ALogE _ t)   = t
getRestricted (ULogE _ _ t) = t
getUnrestricted (CLogE t)     = t
getUnrestricted (ALogE t _)   = t
getUnrestricted (ULogE _ t _) = t

chownLE :: UserId -> LogEntry -> LogEntry
chownLE uid le = ULogE uid (getUnrestricted le) (getRestricted le)

viewLE :: UserId -> LogEntry -> Text
viewLE _      (CLogE t)         = t
viewLE Admin  (ALogE _ t2)      = t2
viewLE _      (ALogE t1 _)      = t1
viewLE viewer (ULogE uid t1 t2) | viewer `isAuthorizationLevel` uid = t2
                                | otherwise                         = t1

mkALogEntry :: String -> String -> LogEntry
mkALogEntry restr unrestr = canonifyLE (ALogE (T.pack restr) (T.pack unrestr))

mkUserLogEntry :: UserId -> String -> String -> LogEntry
mkUserLogEntry uid restr unrestr = canonifyLE (ULogE uid (T.pack restr) (T.pack unrestr))

instance Show LogEntry where
  show (CLogE t)     = T.unpack t
  show (ALogE t _)   = T.unpack t
  show (ULogE _ t _) = T.unpack t

instance IsString LogEntry where
  fromString = CLogE . T.pack

instance Monoid LogEntry where
  mempty = fromString ""
  mappend le1 le2 = canonifyLE $
                    ULogE (getOwner le1 `getCommonUID` getOwner le2)
                          (getUnrestricted le1 `T.append` getUnrestricted le2)
                          (getRestricted le1 `T.append` getRestricted le2)

(<<>) :: String -> LogEntry -> LogEntry
s <<> l = fromString s `mappend` l

(<>>) :: LogEntry -> String -> LogEntry
l <>> s = l `mappend` fromString s

-- | A user dependent Log
type Log = [LogEntry]

instance IsString Log where
  fromString s = [fromString s]

-- | helper function to get the log from the view of an user
viewLog :: UserId -> Log -> Text
viewLog userId l = T.unlines (map (viewLE userId) l)

prefixLE :: UserId -> LogEntry -> LogEntry
prefixLE uid = chownLE uid
             . ((view uid <>> ": ") <>)

-- *** logging helper for the inner circle

logAnEntryI :: (Monad m, MonadTrans t) =>
               LogEntry -> t (WriterT Log m) ()
logAnEntryI = lift
            . W.tell
            . (:[])

loggsAnEntryI :: (Monad m, MonadTrans t) =>
                 UserId -> LogEntry -> t (WriterT Log m) ()
loggsAnEntryI uid = logAnEntryI
                  . prefixLE uid

logI :: (Monad m, MonadTrans t) =>
        String -> t (WriterT Log m) ()
logI = logAnEntryI
     . fromString

loggsI :: (Monad m, MonadTrans t) =>
          UserId -> String -> t (WriterT Log m) ()
loggsI uid = (uid `loggsAnEntryI`)
           . fromString

logggsI :: (Monad m, MonadTrans t) =>
          UserId -> String -> String -> t (WriterT Log m) ()
logggsI uid unrestricted = (uid `loggsAnEntryI`)
                         . mkUserLogEntry uid unrestricted

alogI :: (Monad m, MonadTrans t) =>
        String -> String -> t (WriterT Log m) ()
alogI unrestricted = (Admin `loggsAnEntryI`)
                   . mkALogEntry unrestricted

--------------------------------------------------------------------------------
-- * The View class
--------------------------------------------------------------------------------

class Show a =>
      View a where
  showRestricted :: a -> String
  showRestricted = show

  showUnrestricted :: a -> String
  showUnrestricted = show

  getOwner :: a -> UserId
  getOwner _ = Admin

  view :: a -> LogEntry
  view a = canonifyLE (ULogE (getOwner a)
                            ((T.pack . showUnrestricted) a)
                            ((T.pack . showRestricted) a))

  viewForMe :: UserId -> a -> LogEntry
  viewForMe uid = chownLE uid . view

instance View UserId where
  view (U uid) = fromString (HsT.mkUnderline uid)
  view u       = (fromString . HsT.mkUnderline . show) u

instance View LogEntry where
  showRestricted = T.unpack . getRestricted
  showUnrestricted = T.unpack . getUnrestricted
  getOwner CLogE{}         = Guest
  getOwner ALogE{}         = Admin
  getOwner (ULogE uid _ _) = uid
  view = id

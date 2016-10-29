{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.MetaGame.Types.Core
       ( UserId (..), mkUserId, isAdmin
       , LogEntry (..), (<<>), (<>>)
       , Log, viewLog, logAnEntryI, loggsAnEntryI, logI, loggsI, logggsI, alogI
       , chownLE
       , View (..)
       ) where

import           Prelude hiding (log)
import           Data.String
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W
import qualified System.HsTColors as HsT

--------------------------------------------------------------------------------
-- ** Users and user-related stuff

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
  compare Admin Admin   = EQ
  compare Admin (U _)   = GT
  compare (U _) Admin   = LT
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
-- ** Log

-- data LogLevel
--   = INFO
--   | ERROR
--   | NONE
--   deriving (Eq,Show,Enum,Ord,Bounded)

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
canonifyLE (ULogE Admin t1 t2) = canonifyLE (ALogE t1 t2)
canonifyLE (ULogE Guest _  t2) = CLogE t2
canonifyLE le@(ULogE _  t1 t2) | t1 == t2  = CLogE t1
                               | otherwise = le
canonifyLE le@(ALogE t1 t2)    | t1 == t2  = CLogE t1
                               | otherwise = le
canonifyLE le@(CLogE _)        = le

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
viewLog userId log = T.unlines (map (viewLE userId) log)

-- *** logging helper for the inner circle

logAnEntryI :: (Monad m, MonadTrans t) =>
               LogEntry -> t (WriterT Log m) ()
logAnEntryI = lift
            . W.tell
            . (:[])

loggsAnEntryI :: (Monad m, MonadTrans t) =>
                 UserId -> LogEntry -> t (WriterT Log m) ()
loggsAnEntryI uid = logAnEntryI
                  . chownLE uid
                  . ((view uid <>> ": ") <>)

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
  getOwner _ = Guest

  view :: a -> LogEntry
  view a = canonifyLE (ULogE (getOwner a)
                             ((T.pack . showUnrestricted) a)
                             ((T.pack . showRestricted) a))

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

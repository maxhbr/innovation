module Game.MetaGame.Types.Core
       ( module X
       ) where

import           Data.String
import           Data.Monoid
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as W

import           Game.MetaGame.Types.Core.User as X
import           Game.MetaGame.Types.Core.Log as X
import           Game.MetaGame.Types.Core.Object as X

-- * logging helper for the inner circle
-- ** logging helper for the inner circle

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

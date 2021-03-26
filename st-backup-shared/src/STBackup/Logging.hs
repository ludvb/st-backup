-- | Minimal logging inspired by co-log [1] (which is incompatible with GHCJS)
--
-- [1]: https://github.com/kowainik/co-log
module STBackup.Logging
  ( module STBackup.Logging.Core,
    HasLog (getLogAction),
    LogAction (LogAction, unLogAction),
    log,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Text (Text)
import qualified Data.Text as T
import STBackup.Logging.Core
  ( LogAction (..),
    LogMessage (..),
    Severity (..),
    formatMessage,
    defaultLogAction,
    pattern D,
    pattern E,
    pattern I,
    pattern W,
  )
import Prelude hiding (log)
import Data.Time (getCurrentTime)

class MonadReader env m => HasLog env msg m where
  getLogAction :: env -> LogAction m msg

logMessage :: HasLog env msg m => msg -> m ()
logMessage msg = do
  LogAction logAction <- asks getLogAction
  logAction msg

log :: (HasLog env LogMessage m, MonadIO m) => Severity -> String -> m ()
log msgSeverity msgText = do
  msgTime <- liftIO getCurrentTime
  logMessage LogMessage {..}

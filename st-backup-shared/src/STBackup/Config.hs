{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module STBackup.Config
  ( ApiServerConfig (..),
    BackupService (..),
    HasLogLevel (..),
    SmtpConfig (..),
    TivoliConfig (..),
    WebClientConfig (..),
    WebServerConfig (..),
  )
where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import qualified Data.Map.Strict as MS
import STBackup.Logging
  ( HasLog (getLogAction),
    LogAction (..),
    LogMessage (..),
    Severity,
    defaultLogAction,
    pattern D,
    pattern E,
    pattern I,
    pattern W,
  )

data SmtpConfig = SmtpConfig
  { _smtpSenderName :: String,
    _smtpSenderAddress :: String,
    _smtpHost :: String,
    _smtpPort :: Int,
    _smtpUsername :: Maybe String,
    _smtpPassword :: Maybe String
  }

data TivoliConfig = TivoliConfig
  { _tivoliLocalDatabase :: FilePath,
    _tivoliStorageVolume :: FilePath,
    _tivoliStoragePath :: FilePath
  }

data BackupService = Tivoli

data ApiServerConfig = ApiServerConfig
  { _apiServerDataDirs :: MS.Map String FilePath,
    _apiServerRestoreDir :: FilePath,
    _apiServerTrashDir :: FilePath,
    _apiServerGPGReceiver :: String,
    _apiServerGPGPassphraseFile :: Maybe FilePath,
    _apiServerPort :: Int,
    _apiServerLogLevel :: Severity,
    _apiServerLogFile :: Maybe FilePath,
    _apiServerSmtpConfig :: SmtpConfig,
    _apiServerBackupService :: BackupService,
    _apiServerTivoliConfig :: TivoliConfig
  }

data WebServerConfig = WebServerConfig
  { _webServerPort :: Int,
    _webServerLogLevel :: Severity
  }

data WebClientConfig = WebClientConfig
  { _webClientLogLevel :: Severity
  }

class HasLogLevel e where
  getLogLevel :: e -> Severity

instance HasLogLevel ApiServerConfig where
  getLogLevel = _apiServerLogLevel

instance HasLogLevel WebServerConfig where
  getLogLevel = _webServerLogLevel

instance HasLogLevel WebClientConfig where
  getLogLevel = _webClientLogLevel

instance
  (MonadIO m, MonadReader env m, HasLogLevel env) =>
  HasLog env LogMessage m
  where
  getLogAction :: env -> LogAction m LogMessage
  getLogAction config =
    let logLevel' = getLogLevel config
     in LogAction $ \msg@LogMessage {..} -> do
          if msgSeverity >= logLevel'
            then unLogAction defaultLogAction msg
            else pure ()

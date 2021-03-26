{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module STBackup.Config.Toml
  ( ServerConfig (..),
    readConfig,
    readApiServerConfig,
    readWebServerConfig,
  )
where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import qualified Data.Text as T
import STBackup.Config
  ( ApiServerConfig (..),
    BackupService (..),
    TivoliConfig (..),
    SmtpConfig (..),
    WebServerConfig (..),
  )
import STBackup.Logging
  ( HasLog,
    LogAction (..),
    Severity,
    defaultLogAction,
    pattern D,
    pattern E,
    pattern I,
    pattern W,
  )
import qualified Toml

data ServerConfig = ServerConfig
  { _apiServerConfig :: ApiServerConfig,
    _webServerConfig :: WebServerConfig
  }

severityCodec :: Toml.Key -> Toml.TomlCodec Severity
severityCodec =
  Toml.match
    Toml.BiMap
      { Toml.forward = forward,
        Toml.backward = backward
      }
  where
    forward E = Right . Toml.AnyValue . Toml.Integer $ 0
    forward W = Right . Toml.AnyValue . Toml.Integer $ 1
    forward I = Right . Toml.AnyValue . Toml.Integer $ 2
    forward D = Right . Toml.AnyValue . Toml.Integer $ 3
    backward (Toml.AnyValue x) =
      let x' = Toml.matchInteger x
       in case x' of
            Right 0 -> Right E
            Right 1 -> Right W
            Right 2 -> Right I
            Right 3 -> Right D
            Right _ -> Left $ Toml.ArbitraryError "Invalid log level"
            Left e -> Left $ Toml.WrongValue e

smtpCodec :: Toml.TomlCodec SmtpConfig
smtpCodec =
  SmtpConfig
    <$> Toml.diwrap (Toml.string "sender-name") Toml..= _smtpSenderName
    <*> Toml.diwrap (Toml.string "sender-address") Toml..= _smtpSenderAddress
    <*> Toml.diwrap (Toml.string "host") Toml..= _smtpHost
    <*> Toml.diwrap (Toml.int "port") Toml..= _smtpPort
    <*> Toml.dioptional (Toml.diwrap (Toml.string "username")) Toml..= _smtpUsername
    <*> Toml.dioptional (Toml.diwrap (Toml.string "password")) Toml..= _smtpPassword

tivoliCodec :: Toml.TomlCodec TivoliConfig
tivoliCodec =
  TivoliConfig
    <$> Toml.diwrap (Toml.string "local-database") Toml..= _tivoliLocalDatabase
    <*> Toml.diwrap (Toml.string "storage-volume") Toml..= _tivoliStorageVolume
    <*> Toml.diwrap (Toml.string "storage-path") Toml..= _tivoliStoragePath

backupServiceCodec :: Toml.TomlCodec BackupService
backupServiceCodec =
  Toml.dimatch
    (\case Tivoli -> Just "tivoli")
    (\case "tivoli" -> Tivoli)
    (Toml.string "backup-service")

apiServerConfigCodec :: Toml.TomlCodec ApiServerConfig
apiServerConfigCodec =
  ApiServerConfig
    <$> Toml.map (Toml.string "name") (Toml.string "path") "data-dirs" Toml..= _apiServerDataDirs
    <*> Toml.diwrap (Toml.string "restore-dir") Toml..= _apiServerRestoreDir
    <*> Toml.diwrap (Toml.string "trash-dir") Toml..= _apiServerTrashDir
    <*> Toml.diwrap (Toml.string "gpg-receiver") Toml..= _apiServerGPGReceiver
    <*> Toml.dioptional (Toml.diwrap (Toml.string "gpg-passphrase-file")) Toml..= _apiServerGPGPassphraseFile
    <*> Toml.int "port" Toml..= _apiServerPort
    <*> severityCodec "log-level" Toml..= _apiServerLogLevel
    <*> Toml.dioptional (Toml.diwrap (Toml.string "log-file")) Toml..= _apiServerLogFile
    <*> Toml.table smtpCodec "smtp" Toml..= _apiServerSmtpConfig
    <*> backupServiceCodec Toml..= _apiServerBackupService
    <*> Toml.table tivoliCodec "tivoli" Toml..= _apiServerTivoliConfig

webServerConfigCodec :: Toml.TomlCodec WebServerConfig
webServerConfigCodec =
  WebServerConfig
    <$> Toml.int "port" Toml..= _webServerPort
    <*> severityCodec "log-level" Toml..= _webServerLogLevel

configCodec :: Toml.TomlCodec ServerConfig
configCodec =
  ServerConfig
    <$> Toml.table apiServerConfigCodec "server" Toml..= _apiServerConfig
    <*> Toml.table webServerConfigCodec "web" Toml..= _webServerConfig

readConfig :: FilePath -> IO ServerConfig
readConfig configPath = do
  decodedConfig <- Toml.decodeFileEither configCodec configPath
  case decodedConfig of
    Left es -> throw . userError . T.unpack $ Toml.prettyTomlDecodeErrors es
    Right config -> pure config

readApiServerConfig :: FilePath -> IO ApiServerConfig
readApiServerConfig configPath = do
  config <- readConfig configPath
  pure (_apiServerConfig config)

readWebServerConfig :: FilePath -> IO WebServerConfig
readWebServerConfig configPath = do
  config <- readConfig configPath
  pure (_webServerConfig config)

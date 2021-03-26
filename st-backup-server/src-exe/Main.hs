{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Backup (backup, getBackupFiles)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadIO (..))
import Data.Function ((&))
import qualified Data.Map.Strict as MS
import Files (getLocalFiles)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
    (<**>),
  )
import Persons (addPerson, getPersons)
import Polysemy (Embed, Members, Sem)
import Polysemy.Final
import Polysemy.Reader (Reader, asks, runReader)
import Projects (addProject, getProjects)
import Restore (restore)
import STBackup.API (API)
import STBackup.Backend.Database
  ( Database,
    unDatabaseInterpreter,
  )
import STBackup.Backend.Database.Tivoli (interpretWithTivoli)
import STBackup.Backend.FileLocker
  ( FileLocker,
    unFileLockerInterpreter,
  )
import STBackup.Backend.FileLocker.Naive (interpretNaiveFileLocker)
import qualified STBackup.Config as Config
import STBackup.Config.Toml (readApiServerConfig)
import STBackup.Logging.Core (logToFile)
import STBackup.Logging.Effect
  ( Log,
    LogMessage,
    defaultLogAction,
    injectLog,
    munchLog,
    unLogInjector,
  )
import Servant (Application, Proxy (..), serve, type (:<|>) ((:<|>)))

app ::
  Members
    '[ Embed IO,
       Final IO,
       Log LogMessage,
       Database,
       FileLocker lock,
       Reader Config.ApiServerConfig
     ]
    r =>
  (forall a. Sem r a -> IO a) ->
  Application
app runInterpreter =
  let filesAPI =
        liftIO
          ( runInterpreter $ do
              dataDirs <- asks Config._apiServerDataDirs
              concat <$> forM (MS.toList dataDirs) (uncurry getLocalFiles)
          )
          :<|> liftIO (runInterpreter getBackupFiles)
      backupAPI = pure . backup runInterpreter
      restoreAPI = pure . restore runInterpreter
      projectsAPI =
        liftIO (runInterpreter getProjects)
          :<|> liftIO . runInterpreter . addProject
      personsAPI =
        liftIO (runInterpreter getPersons)
          :<|> liftIO . runInterpreter . addPerson
   in serve
        (Proxy @API)
        ( filesAPI
            :<|> backupAPI
            :<|> restoreAPI
            :<|> projectsAPI
            :<|> personsAPI
        )

runServer :: MonadIO m => Config.ApiServerConfig -> m ()
runServer config@Config.ApiServerConfig {..} = do
  let databaseInterpreter = case _apiServerBackupService of
        Config.Tivoli -> interpretWithTivoli _apiServerTivoliConfig

  let fileLockerInterpreter = interpretNaiveFileLocker
      consoleLogger = injectLog defaultLogAction
      maybeFileLogger = injectLog . logToFile <$> _apiServerLogFile

  liftIO . putStrLn $ "Starting API provider on port " <> show _apiServerPort
  let settings =
        defaultSettings
          & setPort _apiServerPort
          & setTimeout (24 * 60 * 60)
  liftIO . runSettings settings $
    app
      ( runFinal
          . embedToFinal
          . runReader config
          . munchLog
          . unLogInjector consoleLogger
          . ( case maybeFileLogger of
                Just fileLogger -> unLogInjector fileLogger
                _ -> id
            )
          . unDatabaseInterpreter databaseInterpreter
          . unFileLockerInterpreter fileLockerInterpreter
      )

data CommandLineArgs = CommandLineArgs
  { _configFile :: FilePath
  }
  deriving (Show)

optionsParser :: Parser CommandLineArgs
optionsParser =
  CommandLineArgs
    <$> strOption (long "config-file" <> short 'c' <> metavar "CONFIG FILE")

main :: IO ()
main = do
  options <-
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "ST Backup API server")

  readApiServerConfig (_configFile options) >>= runServer

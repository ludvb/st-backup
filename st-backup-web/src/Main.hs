{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Miso
  ( App
      ( App,
        events,
        initialAction,
        logLevel,
        model,
        mountPoint,
        subs,
        update,
        view
      ),
    LogLevel (Off),
    defaultEvents,
    startApp,
  )
import qualified STBackup.Config as Config
import qualified STBackup.Logging as Logging
import STBackup.Web (run)
import STBackup.Web.Controller (getUpdater)
import qualified STBackup.Web.Model as Model
import STBackup.Web.View (view)

#ifdef RELEASE
config :: Config.WebClientConfig
config = Config.WebClientConfig {
  Config._webClientLogLevel = Logging.W
  }
#else
config :: Config.WebClientConfig
config = Config.WebClientConfig {
  Config._webClientLogLevel = Logging.D
  }
#endif

main :: IO ()
main = do
  updater <- runReaderT getUpdater config
  run $
    Miso.startApp
      Miso.App
        { Miso.model =
            Model.Model
              { Model._files = HashMap.fromList [],
                Model._selectedFiles = HashSet.fromList [],
                Model._backups = HashMap.fromList [],
                Model._projects = HashMap.fromList [],
                Model._persons = HashMap.fromList [],
                Model._dialog = Nothing,
                Model._state = Model.Upload
              },
          Miso.initialAction =
            Model.RunActions
              [ Model.UpdateProjects,
                Model.UpdatePersons,
                Model.SetState Model.Upload
              ],
          Miso.update = updater,
          Miso.view = view,
          Miso.mountPoint = Nothing,
          Miso.events = Miso.defaultEvents,
          Miso.subs = [],
          Miso.logLevel = Miso.Off
        }

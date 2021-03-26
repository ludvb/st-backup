module Restore where

import Common
  ( ConsumeStepIO,
    SourceIOEffect,
    runFailAsError,
    runSourceIOEffect,
    setStage,
    yieldProgressMessage,
    yieldSafely,
  )
import Control.Lens ((%~))
import Data.Function ((&))
import Polysemy (Embed, Members, Sem, embed)
import Polysemy.Error (runError, throw)
import Polysemy.Reader (Reader, asks)
import Polysemy.State (runState)
import qualified STBackup.API as API
import STBackup.Backend.Database (Database)
import qualified STBackup.Backend.Database as Database
import STBackup.Backend.FileLocker (FileLocker)
import STBackup.Common (decrypt)
import qualified STBackup.Config as Config
import STBackup.Logging.Effect (Log, LogMessage)
import Servant (SourceIO)
import qualified Servant.Types.SourceT as S
import System.Directory (removeFile, createDirectoryIfMissing)
import System.FilePath.Lens (extension)
import Prelude hiding (log)

data RestoreStage
  = StageDownloading
  | StageDecrypting
  | StageDone
  deriving (Eq)

instance Show RestoreStage where
  show StageDownloading = "Downloading archive from backup server"
  show StageDecrypting = "Decrypting archive"
  show StageDone = "Restore completed successfully"

restore ::
  Members
    '[ Embed IO,
       Log LogMessage,
       Database,
       FileLocker lock,
       Reader Config.ApiServerConfig
     ]
    r =>
  (forall a. Sem r a -> IO a) ->
  Integer ->
  SourceIO API.RestoreResponse
restore interpret backupId =
  S.SourceT $ interpret . doRestoreWrapper
  where
    doRestoreWrapper ::
      Members
        '[ Embed IO,
           Log LogMessage,
           Database,
           Reader Config.ApiServerConfig
         ]
        r =>
      ConsumeStepIO API.RestoreResponse b ->
      Sem r b
    doRestoreWrapper = runSourceIOEffect $ do
      result <- performRestore
      case result of
        Right _ -> pure ()
        Left e -> yieldSafely (Left e) >> pure ()

    performRestore ::
      Members
        '[ Embed IO,
           Log LogMessage,
           Database,
           SourceIOEffect API.RestoreResponse b,
           Reader Config.ApiServerConfig
         ]
        r =>
      Sem r (Either (API.ResponseError String) ())
    performRestore =
      runError
        . (snd <$>)
        . runState
          ( StageDownloading,
            [ StageDownloading,
              StageDecrypting,
              StageDone
            ]
          )
        $ do
          yieldProgressMessage (API.LogInfo "Restoring bakup data")

          restoreDir <- asks Config._apiServerRestoreDir
          embed $ createDirectoryIfMissing True restoreDir

          eitherGpgRestorePath <-
            Database.restoreBackup restoreDir backupId yieldProgressMessage

          gpgRestorePath <- case eitherGpgRestorePath of
            Left e -> throw (API.ServerError e)
            Right x -> pure x

          setStage StageDecrypting
          let archiveRestorePath =
                gpgRestorePath & extension %~ (\x -> if x == ".gpg" then "" else x)
          runFailAsError API.ServerError $
            decrypt gpgRestorePath archiveRestorePath $ \msg -> do
              yieldProgressMessage msg

          embed $ removeFile gpgRestorePath

          yieldProgressMessage $
            API.LogInfo ("Backup restored to " <> show archiveRestorePath)
          setStage StageDone

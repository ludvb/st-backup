{-# LANGUAGE OverloadedStrings #-}

module Backup where

import Common
  ( ConsumeStepIO,
    SourceIOEffect,
    runFailAsError,
    runSourceIOEffect,
    sendMail,
    setStage,
    withFileLocks,
    withTempDir,
    yieldProgressMessage,
    yieldSafely,
  )
import Control.Lens (Traversal', (&), (.~), (?~), _2)
import Control.Lens.Extras (template)
import Control.Lens.Fold ((^..))
import Control.Monad (forM_, unless)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.Hashable (hash)
import Data.List (intercalate)
import qualified Data.Map.Strict as MS
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Data.Time.Clock (UTCTime)
import Persons (getPersons)
import Polysemy
  ( Embed,
    Member,
    Members,
    Sem,
    embed,
  )
import Polysemy.Error (runError, throw)
import Polysemy.Final (Final)
import Polysemy.Reader (Reader, asks)
import Polysemy.Resource (onException, resourceToIOFinal)
import Polysemy.State (modify, runState)
import Projects (getProjects)
import qualified STBackup.API as API
import qualified STBackup.API.Types.Field as Field
import STBackup.Backend.Database (Database)
import qualified STBackup.Backend.Database as Database
import STBackup.Backend.FileLocker (FileLocker)
import STBackup.Common (encrypt, execute)
import qualified STBackup.Config as Config
import STBackup.Logging.Effect
  ( Log,
    LogMessage (LogMessage, msgText, msgTime),
    injectLogId,
    log,
    traceLog,
    pattern E,
  )
import Servant (SourceIO)
import qualified Servant.Types.SourceT as S
import System.Directory (createDirectory, createDirectoryIfMissing, renamePath)
import System.FilePath (takeDirectory, (</>))
import System.Posix (createSymbolicLink)
import System.Process (proc)
import Text.Pretty.Simple (pShow)
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import Prelude hiding (log)

data BackupStage
  = StageValidating
  | StageArchiving
  | StageEncrypting
  | StageSending
  | StageDone
  deriving (Eq)

instance Show BackupStage where
  show StageValidating = "Validating"
  show StageArchiving = "Creating archive"
  show StageEncrypting = "Encrypting archive"
  show StageSending = "Sending archive"
  show StageDone = "Backup completed succesfully"

validateBackup :: Member Database r => API.BackupRequest -> Sem r API.BackupRequest
validateBackup
  x@API.BackupRequest {_backup = (_, API.Metadata {..}), _mail = mail} = do
    (finalState, _) <- runState x doValidation
    pure finalState
    where
      doValidation = do
        -- Validate date is parseable
        let experimentDate =
              parseTimeM
                True
                defaultTimeLocale
                "%Y-%m-%d"
                (Field.fromField _metadataExperimentDate) ::
                Maybe UTCTime
         in unless (isJust experimentDate) $
              modify
                ( &
                    API.backup
                      . _2
                      . API.metadataExperimentDate
                      . Field.error
                      .~ Field.HasError "Date must be formatted as YYYY-MM-DD"
                )

        -- Validate contact person exists
        persons <- getPersons
        unless
          ( Field.fromField _metadataContact
              `elem` [ idx
                       | API.Person {..} <- persons,
                         idx <- toList (Field.fromField _personId)
                     ]
          )
          $ modify
            ( &
                API.backup
                  . _2
                  . API.metadataContact
                  . Field.error
                  .~ Field.HasError "Invalid contact person"
            )

        -- Validate project exists
        projects <- getProjects
        unless
          ( Field.fromField _metadataProject
              `elem` [ idx
                       | API.Project {..} <- projects,
                         idx <- toList (Field.fromField _projectId)
                     ]
          )
          $ modify
            ( &
                API.backup
                  . _2
                  . API.metadataProject
                  . Field.error
                  .~ Field.HasError "Invalid project"
            )

        -- Validate notification e-mail
        unless
          ( (Field.fromField mail =~ ("^[A-Za-z0-9\\.]+@[A-Za-z0-9\\.]+\\.[A-Za-z0-9]+$" :: String))
              || (Field.fromField mail == "")
          )
          $ modify
            ( &
                API.mail
                  . Field.error
                  .~ Field.HasError "Invalid e-mail"
            )

backup ::
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
  API.BackupRequest ->
  SourceIO API.BackupResponse
backup interpret backupRequest =
  S.SourceT $ interpret . doBackupWrapper
  where
    doBackupWrapper ::
      Members
        '[ Embed IO,
           Final IO,
           Log LogMessage,
           Database,
           FileLocker lock,
           Reader Config.ApiServerConfig
         ]
        r =>
      ConsumeStepIO API.BackupResponse b ->
      Sem r b
    doBackupWrapper = runSourceIOEffect $ do
      backupHash <- do
        currentTime <- embed getCurrentTime
        let h = hash (show currentTime)
         in pure $ printf "%07x" (h `mod` 16 ^ (7 :: Int))
      injectLogId backupHash $ do
        now <- embed getCurrentTime
        let currentDate = formatTime defaultTimeLocale "%Y-%m-%d" now

        validatedRequest@API.BackupRequest {_backup = (files, _), _mail = mailField} <-
          validateBackup $
            backupRequest & API.backup . _2 . API.metadataBackupDate . Field.value ?~ currentDate

        let inputErrors =
              validatedRequest
                ^.. (template :: Traversal' API.BackupRequest Field.ErrorS)
                  . Field._HasError
        if not (null inputErrors)
          then do
            _ <- yieldSafely . Left . API.InputError $ validatedRequest
            pure ()
          else do
            (logMessages, result) <-
              traceLog $ performBackup (currentDate <> "+" <> backupHash) validatedRequest
            let logFileData =
                  BSL.fromStrict . BS.intercalate "\n" $
                    reverse logMessages <&> \LogMessage {..} ->
                      let timeString =
                            BS.pack $ formatTime defaultTimeLocale "[ %Y-%m-%d %H:%M:%S ]" msgTime
                          msgString = BS.strip . BS.pack $ msgText
                       in timeString <> " " <> msgString
            case result of
              Right _ -> do
                let mail = Field.fromField mailField
                 in unless (mail == "") $
                      sendMail
                        mail
                        "Backup successful"
                        ( intercalate
                            "\n\n"
                            [ "The following files were backed up successfully:",
                              intercalate "\n" (("- " <>) . API._fileName <$> files)
                            ]
                        )
                        [("text/plain", "log.txt", logFileData)]
                pure ()
              Left e -> do
                log E e
                let mail = Field.fromField mailField
                 in unless (mail == "") $
                      sendMail
                        mail
                        "Backup failed"
                        ( intercalate
                            "\n\n"
                            [ "The following files did not get backed up:",
                              intercalate "\n" (("- " <>) . API._fileName <$> files),
                              "The error returned was: " <> e,
                              "See attached log file for more information."
                            ]
                        )
                        [("text/plain", "log.txt", logFileData)]
                _ <- yieldSafely . Left . API.ServerError $ e
                pure ()

    performBackup ::
      Members
        '[ Embed IO,
           Final IO,
           Log LogMessage,
           Database,
           FileLocker lock,
           SourceIOEffect API.BackupResponse b,
           Reader Config.ApiServerConfig
         ]
        r =>
      String ->
      API.BackupRequest ->
      Sem r (Either String ())
    performBackup
      backupIdentifier
      API.BackupRequest
        { _backup = validatedBackup@(files, API.Metadata {..})
        } =
        resourceToIOFinal
          . runError
          . flip onException (throw "Unknown error")
          . (snd <$>)
          . runState
            ( StageValidating,
              [ StageValidating,
                StageArchiving,
                StageEncrypting,
                StageSending,
                StageDone
              ]
            )
          $ do
            yieldProgressMessage . API.LogInfo $
              "Starting backup:\n" <> TL.unpack (pShow backupRequest)

            maybeProject <- Database.getProject (Field.fromField _metadataProject)
            API.Project {..} <- case maybeProject of
              Just x' -> pure x'
              Nothing -> throw "Failed to retrieve project data"
            let backupName =
                  [if c == ' ' then '_' else c | c <- Field.fromField _projectName]
                    <> "-"
                    <> backupIdentifier

            trashDir <- asks Config._apiServerTrashDir
            dataDirs <- asks Config._apiServerDataDirs

            let trashPath = trashDir </> backupName
                filePaths = files <&> \API.File {..} -> dataDirs MS.! _fileDir </> _fileName

            runFailAsError id . withFileLocks filePaths $ do
              withTempDir $ \tempDir -> do
                setStage StageArchiving

                forM_ files $ \API.File {..} -> do
                  let linkName = tempDir </> backupName </> _fileDir </> _fileName
                      targetName = dataDirs MS.! _fileDir </> _fileName

                  yieldProgressMessage . API.LogInfo $
                    "Creating symlink " <> linkName <> " → " <> targetName
                  embed $ createDirectoryIfMissing True (takeDirectory linkName)
                  embed $ createSymbolicLink targetName linkName

                let archiveFilePath = tempDir </> backupName <> ".tar.gz"
                _ <-
                  runFailAsError id $
                    execute
                      ( proc
                          "tar"
                          [ "-cahv",
                            "--file=" <> archiveFilePath,
                            "-C",
                            tempDir,
                            backupName
                          ]
                      )
                      yieldProgressMessage

                setStage StageEncrypting
                let gpgFile = archiveFilePath <> ".gpg"
                runFailAsError id $
                  encrypt archiveFilePath gpgFile yieldProgressMessage

                setStage StageSending
                maybeBackupId <-
                  Database.addBackup gpgFile validatedBackup yieldProgressMessage

                case maybeBackupId of
                  Left e -> throw e
                  Right backupId ->
                    yieldProgressMessage . API.LogInfo $
                      "✔️ Data backed up (id: " <> show backupId <> ")"

              embed $ createDirectory trashPath
              forM_ files $ \API.File {..} -> do
                let src = dataDirs MS.! _fileDir </> _fileName
                    dst = trashPath </> _fileDir </> _fileName
                yieldProgressMessage . API.LogInfo $ "Moving file " <> show src <> " to " <> show dst
                embed $ createDirectoryIfMissing True (takeDirectory dst)
                embed $ renamePath src dst

            setStage StageDone

getBackupFiles :: Member Database r => Sem r [API.Backup]
getBackupFiles = Database.getBackups

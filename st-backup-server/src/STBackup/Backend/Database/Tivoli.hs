{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Database interpreter for backing up data with the Tivoli Storage Manager
module STBackup.Backend.Database.Tivoli where

import Control.Lens ((%~), (.~), (^.))
import Control.Monad (forM, forM_)
import Data.Aeson (encode)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import qualified Database.SQLite.Simple as SQL
import GHC.Generics (Generic)
import Polysemy
  ( EffectRow,
    Embed,
    Member,
    Members,
    Sem,
    WithTactics,
    bindT,
    embed,
    getInitialStateT,
    interpretH,
    pureT,
    raise,
  )
import Polysemy.Fail (Fail, runFail)
import Polysemy.Reader
import qualified STBackup.API as API
import STBackup.API.Types.Field (fromField)
import qualified STBackup.API.Types.Field as Field
import STBackup.Backend.Database
  ( Database (..),
    DatabaseInterpreter (DatabaseInterpreter),
    unDatabaseInterpreter,
  )
import qualified STBackup.Backend.Database as Database
import STBackup.Common (encrypt, execute)
import qualified STBackup.Config as Config
import STBackup.Logging (LogMessage)
import STBackup.Logging.Effect (Log)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.FilePath.Lens (extension)
import System.IO (IOMode (WriteMode), withFile)
import System.Process (proc)
import Prelude hiding (log)

data Metadata = Metadata
  { project :: String,
    contact :: String,
    description :: String,
    experimentDate :: String,
    backupDate :: String
  }
  deriving (Show, Eq, Generic)

instance JSON.ToJSON Metadata

apiMetadataToMetadata ::
  Members '[Database.Database, Embed IO, Fail] r =>
  API.Metadata ->
  Sem r Metadata
apiMetadataToMetadata API.Metadata {..} = do
  maybeProject <- Database.getProject (fromField _metadataProject)
  API.Project {..} <- case maybeProject of
    Just a -> pure a
    Nothing -> fail ("Unable to retrieve project id " <> show _metadataProject)
  maybeContact <-
    Database.getPerson (fromField _metadataContact)
  API.Person {..} <- case maybeContact of
    Just a -> pure a
    Nothing -> fail ("Unable to retrieve person id " <> show _metadataContact)
  currentTime <- embed getCurrentTime
  let backupDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  pure $
    Metadata
      { project = fromField _projectName,
        contact = fromField _personName,
        description = fromField _metadataDescription,
        experimentDate = fromField _metadataExperimentDate,
        backupDate = backupDate
      }

addPersonWithTivoli ::
  Member (Embed IO) r =>
  Config.TivoliConfig ->
  API.Person ->
  Sem r Integer
addPersonWithTivoli Config.TivoliConfig {..} p = do
  result <- embed $
    SQL.withConnection _tivoliLocalDatabase $ \conn -> do
      SQL.execute conn "INSERT INTO persons (name) VALUES (?)" [Field.fromField (p ^. API.personName)]
      SQL.query_ conn "SELECT last_insert_rowid()" :: IO [SQL.Only Integer]
  case result of
    SQL.Only x : _ -> pure x
    _ -> pure (-1)

getPersonWithTivoli ::
  Member (Embed IO) r =>
  Config.TivoliConfig ->
  Integer ->
  Sem r (Maybe API.Person)
getPersonWithTivoli Config.TivoliConfig {..} idx =
  embed
    . SQL.withConnection _tivoliLocalDatabase
    $ \conn -> do
      entries <-
        SQL.queryNamed
          conn
          "SELECT id, name FROM persons WHERE id = :id"
          [":id" SQL.:= idx] ::
          IO [(Integer, String)]
      case entries of
        [] -> fail "Person does not exist"
        (personId, personName) : _ ->
          pure . Just $
            API.Person
              { API._personId = Field.toField (Just personId),
                API._personName = Field.toField personName
              }

getPersonsWithTivoli ::
  Member (Embed IO) r =>
  Config.TivoliConfig ->
  Sem r [API.Person]
getPersonsWithTivoli Config.TivoliConfig {..} =
  embed $
    SQL.withConnection _tivoliLocalDatabase $ \conn -> do
      entries <-
        SQL.query_
          conn
          "SELECT id, name FROM persons" ::
          IO [(Integer, String)]
      pure $
        entries <&> \(personId, personName) ->
          API.Person
            { API._personId = Field.toField (Just personId),
              API._personName = Field.toField personName
            }

addProjectWithTivoli ::
  Member (Embed IO) r =>
  Config.TivoliConfig ->
  API.Project ->
  Sem r Integer
addProjectWithTivoli Config.TivoliConfig {..} p = do
  result <- embed $
    SQL.withConnection _tivoliLocalDatabase $ \conn -> do
      SQL.execute conn "INSERT INTO projects (name) VALUES (?)" [Field.fromField (p ^. API.projectName)]
      SQL.query_ conn "SELECT last_insert_rowid()" :: IO [SQL.Only Integer]
  case result of
    SQL.Only x : _ -> pure x
    _ -> pure (-1)

getProjectWithTivoli ::
  Member (Embed IO) r =>
  Config.TivoliConfig ->
  Integer ->
  Sem r (Maybe API.Project)
getProjectWithTivoli Config.TivoliConfig {..} idx =
  embed
    . SQL.withConnection _tivoliLocalDatabase
    $ \conn -> do
      entries <-
        SQL.queryNamed
          conn
          "SELECT id, name FROM projects WHERE id = :id"
          [":id" SQL.:= idx] ::
          IO [(Integer, String)]
      case entries of
        [] -> fail "Project does not exist"
        (projectId, projectName) : _ ->
          pure . Just $
            API.Project
              { API._projectId = Field.toField (Just projectId),
                API._projectName = Field.toField projectName
              }

getProjectsWithTivoli ::
  Member (Embed IO) r =>
  Config.TivoliConfig ->
  Sem r [API.Project]
getProjectsWithTivoli Config.TivoliConfig {..} = do
  embed $
    SQL.withConnection _tivoliLocalDatabase $ \conn -> do
      entries <-
        SQL.query_
          conn
          "SELECT id, name FROM projects" ::
          IO [(Integer, String)]
      pure $
        entries <&> \(projectId, projectName) ->
          API.Project
            { API._projectId = Field.toField (Just projectId),
              API._projectName = Field.toField projectName
            }

addBackupWithTivoli ::
  forall (f :: * -> *) (r :: EffectRow) m.
  (Functor f, Members '[Log LogMessage, Embed IO, Reader Config.ApiServerConfig] r, Applicative m) =>
  Config.TivoliConfig ->
  FilePath ->
  API.Backup ->
  (API.LogMessage -> m ()) ->
  Sem (WithTactics Database f m r) (f (Either String Integer))
addBackupWithTivoli
  c@Config.TivoliConfig {..}
  archivePath
  (files, apiMetadata@API.Metadata {..})
  consumeLogMessage = do
    initialState <- getInitialStateT
    state <- embed $ newIORef initialState

    result <- runFail $ do
      let consumeLogMessage' = \msg -> do
            curState <- embed $ readIORef state
            constructMsg <- raise (bindT (pure . const msg))
            msg' <- raise . raise . unDatabaseInterpreter (interpretWithTivoli c) $ constructMsg curState
            c' <- raise (bindT consumeLogMessage)
            newState <- raise . raise . unDatabaseInterpreter (interpretWithTivoli c) $ c' msg'
            embed $ writeIORef state newState

      let archiveDirectory = takeDirectory archivePath
          sourceDirectory = archiveDirectory </> _tivoliStoragePath
          archiveName = takeFileName archivePath
          metadataName = archiveName & extension .~ "metadata.json"
          metadataNameGpg = metadataName & extension %~ (<> ".gpg")
          metadataPath = sourceDirectory </> metadataName
          metadataPathGpg = sourceDirectory </> metadataNameGpg
          archiveStoragePath = _tivoliStorageVolume </> _tivoliStoragePath </> archiveName
          metadataStoragePath = _tivoliStorageVolume </> _tivoliStoragePath </> metadataNameGpg

      metadata <- unDatabaseInterpreter (interpretWithTivoli c) $ apiMetadataToMetadata apiMetadata

      embed $ createDirectoryIfMissing True sourceDirectory
      embed $ renameFile archivePath (sourceDirectory </> archiveName)
      embed $ withFile (sourceDirectory </> metadataName) WriteMode (`BSL.hPut` encode metadata)
      encrypt metadataPath metadataPathGpg consumeLogMessage'

      -- Upload archive
      execute
        (proc "dsmc" ["archive", archiveStoragePath, "-snapshotroot=" <> archiveDirectory])
        consumeLogMessage'

      -- Upload metadata
      execute
        (proc "dsmc" ["archive", metadataStoragePath, "-snapshotroot=" <> archiveDirectory])
        consumeLogMessage'

      -- Save metadata to local SQL cache
      embed $
        SQL.withConnection _tivoliLocalDatabase $ \conn -> do
          SQL.execute
            conn
            ( "INSERT INTO metadata"
                <> " (project, contact, description, experiment_date, backup_date, archive_file, metadata_file)"
                <> " VALUES (?, ?, ?, ?, ?, ?)"
            )
            ( fromField _metadataProject,
              fromField _metadataContact,
              fromField _metadataDescription,
              fromField _metadataExperimentDate,
              fromMaybe "" (fromField _metadataBackupDate),
              archiveStoragePath,
              metadataStoragePath
            )
          (SQL.Only metadataId) : _ <-
            SQL.query_
              conn
              "SELECT last_insert_rowid()" ::
              IO [SQL.Only Integer]
          forM_ files $ \API.File {..} -> do
            SQL.execute
              conn
              "INSERT INTO files (metadata, filedir, filename, modified, filetype) VALUES (?, ?, ?, ?, ?)"
              ( metadataId,
                _fileDir,
                _fileName,
                _fileModified,
                show _fileType
              )
          pure metadataId

    finalState <- embed $ readIORef state
    let return' = \f -> do
          r <- bindT (\_ -> pure f)
          raise . unDatabaseInterpreter (interpretWithTivoli c) . r $ finalState
     in case result of
          Left e -> return' (Left e)
          Right idx -> return' (Right idx)

restoreBackupWithTivoli ::
  forall (f :: * -> *) (r :: EffectRow) (m :: * -> *).
  ( Functor f,
    Members '[Log LogMessage, Embed IO, Reader Config.ApiServerConfig] r,
    Applicative m
  ) =>
  Config.TivoliConfig ->
  FilePath ->
  Integer ->
  (API.LogMessage -> m ()) ->
  Sem (WithTactics Database f m r) (f (Either String FilePath))
restoreBackupWithTivoli
  c@Config.TivoliConfig {..}
  restoreDir
  metadataId
  consumeLogMessage = do
    initialState <- getInitialStateT
    state <- embed $ newIORef initialState

    result <- runFail $ do
      let consumeLogMessage' = \msg -> do
            curState <- embed $ readIORef state
            constructMsg <- raise (bindT (pure . const msg))
            msg' <- raise . raise . unDatabaseInterpreter (interpretWithTivoli c) $ constructMsg curState
            c' <- raise (bindT consumeLogMessage)
            newState <- raise . raise . unDatabaseInterpreter (interpretWithTivoli c) $ c' msg'
            embed $ writeIORef state newState

      -- Get archive path
      (SQL.Only archiveStoragePath) : _ <- embed $
        SQL.withConnection _tivoliLocalDatabase $ \conn -> do
          SQL.query
            conn
            "SELECT archive_file FROM metadata WHERE id = ?"
            [metadataId] ::
            IO [SQL.Only String]

      let archiveRestoredPath = restoreDir </> takeFileName archiveStoragePath

      -- Retrieve backup
      execute
        (proc "dsmc" ["retrieve", "-replace=no", archiveStoragePath, archiveRestoredPath])
        consumeLogMessage'

      consumeLogMessage' (API.LogInfo ("Tivoli backup restored (id: " <> show metadataId <> ")"))

      pure archiveRestoredPath

    finalState <- embed $ readIORef state
    let return' = \f -> do
          r <- bindT (\_ -> pure f)
          raise . unDatabaseInterpreter (interpretWithTivoli c) . r $ finalState
    case result of
      Left e -> return' (Left e)
      Right idx -> return' (Right idx)

getBackupsWithTivoli ::
  Members '[Embed IO, Log LogMessage] r =>
  Config.TivoliConfig ->
  Sem r [API.Backup]
getBackupsWithTivoli Config.TivoliConfig {..} = do
  conn <- embed $ SQL.open _tivoliLocalDatabase
  entries <-
    embed
      ( SQL.query_ conn "SELECT id, project, contact, description, backup_date, experiment_date FROM metadata" ::
          IO [(Integer, Integer, Integer, String, String, String)]
      )
  forM entries $ \(backupId, project, contact, description, backupDate, experimentDate) -> do
    files <-
      embed
        ( SQL.queryNamed
            conn
            "SELECT filedir, filename, modified, filetype FROM files WHERE metadata = :id"
            [":id" SQL.:= backupId] ::
            IO [(FilePath, String, UTCTime, String)]
        )
    wrappedFiles <- forM files $ \(filedir, filename, modified, _filetype) ->
      pure
        API.File
          { API._fileName = filename,
            API._fileDir = filedir,
            API._fileModified = modified,
            API._fileType = API.Other
          }
    pure
      ( wrappedFiles,
        API.Metadata
          { API._metadataContact = Field.toField contact,
            API._metadataExperimentDate = Field.toField experimentDate,
            API._metadataBackupDate = Field.toField (Just backupDate),
            API._metadataDescription = Field.toField description,
            API._metadataProject = Field.toField project,
            API._metadataId = Field.toField (Just backupId)
          }
      )

interpretWithTivoli ::
  Members '[Embed IO, Log LogMessage, Reader Config.ApiServerConfig] r =>
  Config.TivoliConfig ->
  DatabaseInterpreter r
interpretWithTivoli c = DatabaseInterpreter $
  interpretH $ \case
    GetPersons -> getPersonsWithTivoli c >>= pureT
    GetPerson idx -> getPersonWithTivoli c idx >>= pureT
    AddPerson p -> addPersonWithTivoli c p >>= pureT
    GetProjects -> getProjectsWithTivoli c >>= pureT
    GetProject idx -> getProjectWithTivoli c idx >>= pureT
    AddProject p -> addProjectWithTivoli c p >>= pureT
    GetBackups -> getBackupsWithTivoli c >>= pureT
    RestoreBackup d idx g -> restoreBackupWithTivoli c d idx g
    AddBackup b m g -> addBackupWithTivoli c b m g

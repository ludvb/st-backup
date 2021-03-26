{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module STBackup.API where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import STBackup.API.Types.Field (Field)
import Servant.API
  ( Get,
    JSON,
    Post,
    QueryParam',
    ReqBody,
    Required,
    Strict,
    type (:<|>),
    type (:>),
  )
import Servant.API.Stream (NewlineFraming, SourceIO, StreamGet, StreamPost)

data Filetype
  = Archive
  | Binary
  | Directory
  | Image
  | Other
  | Text
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON Filetype

instance FromJSON Filetype

data ResponseError a = InputError a | ServerError String
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON a => ToJSON (ResponseError a)

instance FromJSON a => FromJSON (ResponseError a)

data File = File
  { _fileName :: String,
    _fileDir :: FilePath,
    _fileModified :: UTCTime,
    _fileType :: Filetype
  }
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON File

instance FromJSON File

$(makeLenses ''File)

data Metadata = Metadata
  { -- | Contact person id
    _metadataContact :: Field Integer,
    -- | Project id
    _metadataProject :: Field Integer,
    -- | Text description
    _metadataDescription :: Field String,
    -- | Date of experiment
    _metadataExperimentDate :: Field String,
    -- | Date of backup
    _metadataBackupDate :: Field (Maybe String),
    -- | Experiment id
    -- This field references an existing backup stored on the backup server.
    -- When submitting a new backup, it can be left as 'Nothing'.
    _metadataId :: Field (Maybe Integer)
  }
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON Metadata

instance FromJSON Metadata

instance Hashable Metadata

$(makeLenses ''Metadata)

type Backup = ([File], Metadata)

{- ORMOLU_DISABLE -}
type FileAPI =
         "local"  :> Get '[JSON] [File]
    :<|> "backup" :> Get '[JSON] [Backup]
{- ORMOLU_ENABLE -}

data LogMessage
  = LogInfo String
  | LogOut String
  | LogErr String
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON LogMessage

instance FromJSON LogMessage

$(makePrisms ''LogMessage)

data Progress = Progress
  { _progressComplete :: Float,
    _progressStage :: String,
    _progressTime :: UTCTime,
    _progressLog :: Maybe LogMessage
  }
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON Progress

instance FromJSON Progress

$(makeLenses ''Progress)

data BackupRequest = BackupRequest
  { _backup :: Backup,
    _mail :: Field String
  }
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON BackupRequest

instance FromJSON BackupRequest

$(makeLenses ''BackupRequest)

type BackupResponse = Either (ResponseError BackupRequest) Progress

type BackupAPI =
  ReqBody '[JSON] BackupRequest
    :> StreamPost NewlineFraming JSON (SourceIO BackupResponse)

type RestoreResponse = Either (ResponseError String) Progress

type RestoreAPI =
  QueryParam' '[Required, Strict] "id" Integer
    :> StreamGet NewlineFraming JSON (SourceIO RestoreResponse)

data Project = Project
  { -- | Project name
    _projectName :: Field String,
    -- | Project id
    -- This field references an existing project stored on the backup server.
    -- When submitting a new project, it should be left as 'Nothing'.
    _projectId :: Field (Maybe Integer)
  }
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON Project

instance FromJSON Project

$(makeLenses ''Project)

{- ORMOLU_DISABLE -}
type ProjectsAPI =
       "get_all"                            :> Get  '[JSON] [Project]
  :<|> "add"     :> ReqBody '[JSON] Project :> Post '[JSON] (Either (ResponseError Project) Integer)
{- ORMOLU_ENABLE -}

data Person = Person
  { -- | Person name
    _personName :: Field String,
    -- | Person id
    -- This field references an existing person stored on the backup server.
    -- When submitting a new person, it should be left as 'Nothing'.
    _personId :: Field (Maybe Integer)
  }
  deriving (Eq, Show, Generic, Data, Typeable)

instance ToJSON Person

instance FromJSON Person

$(makeLenses ''Person)

{- ORMOLU_DISABLE -}
type PersonsAPI =
       "get_all"                           :> Get  '[JSON] [Person]
  :<|> "add"     :> ReqBody '[JSON] Person :> Post '[JSON] (Either (ResponseError Person) Integer)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
type API =
       "files"    :> FileAPI
  :<|> "backup"   :> BackupAPI
  :<|> "restore"  :> RestoreAPI
  :<|> "projects" :> ProjectsAPI
  :<|> "persons"  :> PersonsAPI
{- ORMOLU_ENABLE -}

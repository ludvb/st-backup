{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model
module STBackup.Web.Model where

import Control.Lens
  ( makeLenses,
    makePrisms,
  )
import Data.Data
  ( Data,
    Typeable,
  )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified STBackup.API as API
import qualified Servant.Types.SourceT as S

data Error
  = RuntimeError String
  | UserError String
  deriving (Eq, Data, Typeable)

instance Show Error where
  show (RuntimeError e) = e
  show (UserError e) = "Error: " <> e

$(makePrisms ''Error)

-- | Type wrapping a function to give it naive instances of 'Show' and 'Eq'.
newtype Function a b = Function {unFunction :: a -> b}
  deriving (Typeable)

instance Show (Function a b) where
  show _ = "Function"

instance Eq (Function a b) where
  _ == _ = False

data State
  = Upload
  | Restore
  deriving (Show, Eq, Data, Typeable)

data Symbol
  = NoSymbol
  | CheckMarkSymbol
  | ErrorSymbol
  deriving (Show, Eq, Data, Typeable)

data Notification = Notification
  { _notificationSymbol :: Symbol,
    _notificationMessage :: String
  }
  deriving (Show, Eq, Data, Typeable)

data ProcessStatus
  = ProcessRunning
  | ProcessError String (Function () Action)
  | ProcessDone (Function () Action)
  deriving (Show, Eq, Typeable)

data Dialog
  = NotificationDialog Notification
  | ProgressDialog String [API.Progress] ProcessStatus
  | BackupDialog API.BackupRequest
  | RestoreDialog API.Backup
  | PersonsDialog API.Person Model (Function Integer Action)
  | ProjectsDialog API.Project Model (Function Integer Action)
  | ErrorDialog Error Model
  deriving (Show, Eq, Typeable)

data Model = Model
  { -- | Local files in staging directory
    _files :: HashMap Integer API.File,
    -- | Selected files
    _selectedFiles :: HashSet Integer,
    -- | Remote backups
    _backups :: HashMap Integer API.Backup,
    -- | The registered projects
    _projects :: HashMap Integer API.Project,
    -- | The registered contact persons
    _persons :: HashMap Integer API.Person,
    -- | Dialog
    _dialog :: Maybe Dialog,
    -- | State
    _state :: State
  }
  deriving (Show, Eq, Typeable)

data Action
  = NoOp
  | SetModel Model
  | RunActions [Action]
  | SetBackups (HashMap.HashMap Integer ([API.File], API.Metadata))
  | SetDialog (Maybe Dialog)
  | SetFiles (HashMap.HashMap Integer API.File)
  | SetPersons (HashMap.HashMap Integer API.Person)
  | SetProjects (HashMap.HashMap Integer API.Project)
  | SetState State
  | SpawnBackupDialog
  | SubmitBackupDialog API.BackupRequest
  | SubmitPersonsDialog API.Person Model (Function Integer Action)
  | SubmitProjectsDialog API.Project Model (Function Integer Action)
  | SubmitRestoreDialog API.Backup
  | ToggleSelectedFile Integer
  | UpdateBackups
  | UpdateFiles
  | UpdateProjects
  | UpdatePersons
  | UpdateBackupProgress API.BackupRequest (S.StepT IO API.BackupResponse)
  | UpdateRestoreProgress API.Backup (S.StepT IO API.RestoreResponse)
  deriving (Typeable)

$(makeLenses ''Notification)

$(makePrisms ''ProcessStatus)

$(makePrisms ''Dialog)

$(makeLenses ''Model)

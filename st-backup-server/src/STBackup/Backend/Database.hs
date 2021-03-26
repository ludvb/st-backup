module STBackup.Backend.Database where

import Polysemy (makeSem)
import Polysemy.Internal (InterpreterFor)
import qualified STBackup.API as API

{- ORMOLU_DISABLE -}
data Database m a where
  GetPersons :: Database m [API.Person]
  GetPerson :: Integer -> Database m (Maybe API.Person)
  AddPerson :: API.Person -> Database m Integer

  GetProjects :: Database m [API.Project]
  GetProject :: Integer -> Database m (Maybe API.Project)
  AddProject :: API.Project -> Database m Integer

  GetBackups :: Database m [API.Backup]
  AddBackup ::
    FilePath
    -> API.Backup
    -> (API.LogMessage -> m ())
    -> Database m (Either String Integer)
  RestoreBackup ::
    FilePath
    -> Integer
    -> (API.LogMessage -> m ())
    -> Database m (Either String FilePath)
{- ORMOLU_ENABLE -}

makeSem ''Database

newtype DatabaseInterpreter r = DatabaseInterpreter
  {unDatabaseInterpreter :: InterpreterFor Database r}

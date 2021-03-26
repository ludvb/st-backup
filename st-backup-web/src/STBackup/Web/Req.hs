{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | API helper functions
module STBackup.Web.Req where

import Data.Proxy (Proxy (..))
import Miso (JSM)
import qualified STBackup.API as API
import STBackup.Web.Model (Error (RuntimeError))
import Servant.API (type (:<|>) ((:<|>)))
import qualified Servant.API as S
import Servant.Client.JSaddle
  ( BaseUrl (baseUrlPath),
    ClientM,
    HasClient (Client),
    client,
    getDefaultBaseUrl,
    mkClientEnv,
    runClientM,
  )

getFiles' :: ClientM [API.File]
getBackups' :: ClientM [([API.File], API.Metadata)]
backupFiles' :: Client ClientM API.BackupAPI
restoreFiles' :: Client ClientM API.RestoreAPI
getProjects' :: ClientM [API.Project]
addProject' :: API.Project -> ClientM (Either (API.ResponseError API.Project) Integer)
getPersons' :: ClientM [API.Person]
addPerson' :: API.Person -> ClientM (Either (API.ResponseError API.Person) Integer)
(getFiles' :<|> getBackups')
  :<|> backupFiles'
  :<|> restoreFiles'
  :<|> (getProjects' :<|> addProject')
  :<|> (getPersons' :<|> addPerson') = client (Proxy @API.API)

runQuery :: ClientM a -> JSM (Either Error a)
runQuery q = do
  url <- getDefaultBaseUrl >>= \burl -> pure $ burl {baseUrlPath = "api"}
  response <- runClientM q (mkClientEnv url)
  case response of
    Right result -> pure . Right $ result
    Left clientError ->
      pure . Left . RuntimeError $ "API query failed: " <> show clientError

getFiles :: JSM (Either Error [API.File])
getFiles = runQuery getFiles'

getBackups :: JSM (Either Error [([API.File], API.Metadata)])
getBackups = runQuery getBackups'

getProjects :: JSM (Either Error [API.Project])
getProjects = runQuery getProjects'

getPersons :: JSM (Either Error [API.Person])
getPersons = runQuery getPersons'

addProject :: API.Project -> JSM (Either Error (Either (API.ResponseError API.Project) Integer))
addProject = runQuery . addProject'

addPerson :: API.Person -> JSM (Either Error (Either (API.ResponseError API.Person) Integer))
addPerson = runQuery . addPerson'

backup :: API.BackupRequest -> JSM (Either Error (S.SourceIO API.BackupResponse))
backup = runQuery . backupFiles'

restore :: Integer -> JSM (Either Error (S.SourceIO API.RestoreResponse))
restore = runQuery . restoreFiles'

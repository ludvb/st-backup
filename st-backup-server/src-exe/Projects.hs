module Projects where

import Control.Lens ((&), (.~), (^.), (^..))
import Control.Lens.Traversal (Traversal')
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept, runExceptT, throwE)
import Data.Data.Lens (template)
import Polysemy (Member, Sem)
import qualified STBackup.API as API
import qualified STBackup.API.Types.Field as Field
import qualified STBackup.Backend.Database as Database
import Text.Regex.TDFA ((=~))

getProjects :: Member Database.Database r => Sem r [API.Project]
getProjects = Database.getProjects

addProject ::
  Member Database.Database r =>
  API.Project ->
  Sem r (Either (API.ResponseError API.Project) Integer)
addProject project =
  runExceptT $ do
    let verifiedProject = foldr (\v p -> v p) project verifiers
        errors =
          verifiedProject
            ^.. (template :: Traversal' API.Project Field.ErrorS)
              . Field._HasError
    unless (null errors) (throwE $ API.InputError verifiedProject)
    lift $ Database.addProject verifiedProject
  where
    verifiers = [verifyName]
    verifyName p =
      let result =
            runExcept $ do
              when
                (p ^. API.projectName . Field.value == "")
                (throwE $ p & API.projectName . Field.error .~ Field.HasError "Must be non-empty")
              unless
                ((p ^. API.projectName . Field.value) =~ ("^[A-Za-zÅÄÖåäö ]+$" :: String))
                ( throwE $
                    p & API.projectName . Field.error
                      .~ Field.HasError "Only characters in A-Z, Å, Ä, Ö and space are allowed"
                )
       in case result of
            Left p' -> p'
            Right _ -> p

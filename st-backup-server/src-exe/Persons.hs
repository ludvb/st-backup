module Persons where

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

getPersons :: Member Database.Database r => Sem r [API.Person]
getPersons = Database.getPersons

addPerson ::
  Member Database.Database r =>
  API.Person ->
  Sem r (Either (API.ResponseError API.Person) Integer)
addPerson person =
  runExceptT $ do
    let verifiedPerson = foldr (\v p -> v p) person verifiers
        errors =
          verifiedPerson
            ^.. (template :: Traversal' API.Person Field.ErrorS)
              . Field._HasError
    unless (null errors) (throwE $ API.InputError verifiedPerson)
    lift $ Database.addPerson verifiedPerson
  where
    verifiers = [verifyName]
    verifyName p =
      let result =
            runExcept $ do
              when
                (p ^. API.personName . Field.value == "")
                (throwE $ p & API.personName . Field.error .~ Field.HasError "Must be non-empty")
              unless
                ((p ^. API.personName . Field.value) =~ ("^[A-Za-zÅÄÖåäö ]+$" :: String))
                ( throwE $
                    p & API.personName . Field.error
                      .~ Field.HasError "Only characters in A-Z, Å, Ä, Ö and space are allowed"
                )
       in case result of
            Left p' -> p'
            Right _ -> p

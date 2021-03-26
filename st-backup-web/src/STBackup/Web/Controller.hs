{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Controller
module STBackup.Web.Controller where

import Control.Lens (Traversal', (%~), (&), (.~), (^.))
import Control.Lens.Tuple (_2, _3)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Data.Lens (template)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Maybe (isJust)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import JSDOM (currentWindow)
import JSDOM.Generated.Window (scrollToOpt)
import JSDOM.Types (JSString, ScrollToOptions (ScrollToOptions), toJSVal)
import qualified JavaScript.Object as Object
import Language.Javascript.JSaddle (MonadJSM (..), liftJSM)
import Miso (Effect)
import Miso.Effect (batchEff)
import qualified STBackup.API as API
import qualified STBackup.API.Types.Field as Field
import qualified STBackup.Config as Config
import STBackup.Logging (log, pattern D, pattern E)
import STBackup.Web.Model
  ( Action (..),
    Dialog
      ( BackupDialog,
        ErrorDialog,
        PersonsDialog,
        ProgressDialog,
        ProjectsDialog
      ),
    Error (RuntimeError),
    Function (unFunction),
    Model (..),
    State (Restore, Upload),
    backups,
    dialog,
    files,
    persons,
    projects,
    selectedFiles,
    state,
    _ProgressDialog,
  )
import qualified STBackup.Web.Model as Model
import STBackup.Web.Req
  ( addPerson,
    addProject,
    backup,
    getBackups,
    getFiles,
    getPersons,
    getProjects,
    restore,
  )
import Servant.Types.SourceT (SourceT (unSourceT))
import qualified Servant.Types.SourceT as S
import Prelude hiding (log)

getUpdater ::
  Applicative m =>
  ReaderT Config.WebClientConfig m (Action -> Model -> Effect Action Model)
getUpdater = ReaderT $ \config ->
  let getUpdater' action model =
        let (actions, updatedModel) = getUpdate action model
            actions' = actions <&> \a -> runReaderT a config
         in batchEff updatedModel actions'
   in pure getUpdater'

getUpdate ::
  (Config.HasLogLevel env, MonadReader env m, MonadJSM m) =>
  Action ->
  Model ->
  ([m Action], Model)
getUpdate NoOp m = ([], m)
getUpdate (SetModel m) _ = ([], m)
getUpdate (RunActions as) m = (pure <$> as, m)
getUpdate (SetDialog x) m = ([scrollToTop | isJust x], m & dialog .~ x)
  where
    scrollToTop = do
      maybeWindow <- liftJSM currentWindow
      case maybeWindow of
        Just window -> liftJSM $ do
          scrollToOptions <- do
            obj <- Object.create

            topVal <- toJSVal (0 :: Int)
            Object.setProp "top" topVal obj

            behaviorVal <- toJSVal ("smooth" :: JSString)
            Object.setProp "behavior" behaviorVal obj

            toJSVal obj

          scrollToOpt window . Just . ScrollToOptions $ scrollToOptions
        Nothing -> pure ()
      pure NoOp
getUpdate (SetBackups x) m = ([], m & backups .~ x)
getUpdate (SetFiles x) m = ([], m')
  where
    m' =
      if x == m ^. files
        then m
        else m & files .~ x & selectedFiles .~ HashSet.fromList []
getUpdate (SetProjects x) m = ([], m & projects .~ x)
getUpdate (SetPersons x) m = ([], m & persons .~ x)
getUpdate (SetState x) m = ([sync], m & state .~ x & dialog .~ Nothing)
  where
    sync =
      case x of
        Upload -> pure UpdateFiles
        Restore -> pure UpdateBackups
getUpdate SpawnBackupDialog m = ([setDialog], m)
  where
    setDialog = liftIO $ do
      currentTime <- getCurrentTime
      pure . SetDialog . Just . BackupDialog $
        API.BackupRequest
          ( [ (m ^. files) HashMap.! fileId
              | fileId <- HashSet.toList (m ^. selectedFiles)
            ],
            API.Metadata
              { API._metadataContact = Field.toField (-1),
                API._metadataExperimentDate =
                  Field.toField
                    (formatTime defaultTimeLocale "%Y-%m-%d" currentTime),
                API._metadataDescription = Field.toField "",
                API._metadataProject = Field.toField (-1),
                API._metadataBackupDate = Field.toField Nothing,
                API._metadataId = Field.toField Nothing
              }
          )
          (Field.toField "")
getUpdate (SubmitBackupDialog backupData) m = ([submitBackupDialog], m)
  where
    submitBackupDialog = do
      let backupData' =
            backupData
              & (template :: Traversal' API.BackupRequest Field.ErrorS) .~ Field.NoError
      response <- liftJSM $ backup backupData'
      case response of
        Left e -> do
          log E . show $ e
          pure . SetDialog . Just $ ErrorDialog e m
        Right reader -> liftIO $ do
          steps <- unSourceT reader pure
          now <- getCurrentTime
          pure $
            RunActions
              [ SetDialog . Just $
                  ProgressDialog
                    "backup"
                    [ API.Progress
                        { API._progressComplete = 0.0,
                          API._progressTime = now,
                          API._progressStage = "",
                          API._progressLog = Nothing
                        }
                    ]
                    Model.ProcessRunning,
                UpdateBackupProgress backupData steps
              ]
getUpdate (SubmitPersonsDialog person cancelModel returnFunction) m =
  ([submitPersonsDialog], m)
  where
    submitPersonsDialog = do
      result <- runExceptT runSubmit
      case result of
        Left e -> pure . SetDialog . Just $ ErrorDialog e m
        Right a -> pure a
      where
        runSubmit = do
          let person' = person & (template :: Traversal' API.Person Field.ErrorS) .~ Field.NoError
          result <- liftJSM (addPerson person')
          response <- case result of
            Left e -> throwE e
            Right r -> pure r
          case response of
            Left e -> do
              case e of
                API.ServerError se -> throwE (RuntimeError se)
                API.InputError p ->
                  pure . SetDialog . Just $ PersonsDialog p cancelModel returnFunction
            Right personIdx ->
              pure $
                RunActions
                  [ UpdatePersons,
                    unFunction returnFunction personIdx
                  ]
getUpdate (SubmitProjectsDialog project cancelModel returnFunction) m =
  ([submitProjectsDialog], m)
  where
    submitProjectsDialog = do
      result <- runExceptT runSubmit
      case result of
        Left e -> pure . SetDialog . Just $ ErrorDialog e m
        Right a -> pure a
      where
        runSubmit = do
          let project' = project & (template :: Traversal' API.Project Field.ErrorS) .~ Field.NoError
          result <- liftJSM $ addProject project'
          response <- case result of
            Left e -> throwE e
            Right r -> pure r
          case response of
            Left e -> do
              case e of
                API.ServerError se -> throwE (RuntimeError se)
                API.InputError p ->
                  pure . SetDialog . Just $ ProjectsDialog p cancelModel returnFunction
            Right projectIdx ->
              pure $
                RunActions
                  [ UpdateProjects,
                    unFunction returnFunction projectIdx
                  ]
getUpdate (SubmitRestoreDialog backupData@(_, API.Metadata {..})) m = ([submitRestoreDialog], m)
  where
    submitRestoreDialog = do
      result <- runExceptT runSubmit
      case result of
        Left e -> pure . SetDialog . Just $ ErrorDialog e m
        Right a -> pure a
      where
        runSubmit = do
          backupId <- case Field.fromField _metadataId of
            Just x -> pure x
            Nothing -> throwE (RuntimeError "Backup has no id")
          response <- liftJSM (restore backupId)
          case response of
            Left e -> do
              log E . show $ e
              pure . SetDialog . Just $ ErrorDialog e m
            Right reader -> liftIO $ do
              steps <- unSourceT reader pure
              now <- getCurrentTime
              pure $
                RunActions
                  [ SetDialog . Just $
                      ProgressDialog
                        "Restoring data"
                        [ API.Progress
                            { API._progressComplete = 0.0,
                              API._progressTime = now,
                              API._progressStage = "",
                              API._progressLog = Nothing
                            }
                        ]
                        Model.ProcessRunning,
                    UpdateRestoreProgress backupData steps
                  ]
getUpdate (UpdateBackupProgress _ S.Stop) m = ([finishProgress], m)
  where
    finishProgress =
      pure $
        SetModel $
          m & dialog . traverse . _ProgressDialog . _3
            .~ Model.ProcessDone
              ( Model.Function
                  ( const $
                      RunActions [UpdateFiles, SetDialog Nothing]
                  )
              )
getUpdate (UpdateBackupProgress b (S.Yield (Right p) c)) m =
  ([pure (updateProgress m p), liftJSM scroll, updateNext], m)
  where
    scroll = pure NoOp
    updateNext = pure (UpdateBackupProgress b c)
getUpdate (UpdateBackupProgress b (S.Yield (Left e) _)) m =
  ([handleError e], m)
  where
    handleError (API.InputError x) =
      pure . SetDialog . Just $ BackupDialog x
    handleError (API.ServerError x) =
      pure . SetModel $
        m & dialog . traverse . _ProgressDialog . _3
          .~ Model.ProcessError
            x
            ( Model.Function
                ( const . SetDialog . Just . Model.BackupDialog $
                    b
                      & (template :: Traversal' API.BackupRequest Field.ErrorS)
                      .~ Field.NoError
                )
            )
getUpdate (UpdateBackupProgress b (S.Effect c')) m = ([action], m)
  where
    action = liftIO $ c' >>= \c -> pure $ UpdateBackupProgress b c
getUpdate (UpdateBackupProgress b (S.Skip c)) m =
  getUpdate (UpdateBackupProgress b c) m
getUpdate (UpdateBackupProgress _ (S.Error e)) m =
  ([pure . SetDialog . Just $ ErrorDialog (RuntimeError e) m], m)
getUpdate (UpdateRestoreProgress _ S.Stop) m = ([finishProgress], m)
  where
    finishProgress =
      pure $
        SetModel $
          m & dialog . traverse . _ProgressDialog . _3
            .~ Model.ProcessDone
              ( Model.Function
                  ( const $
                      RunActions [UpdateFiles, SetDialog Nothing]
                  )
              )
getUpdate (UpdateRestoreProgress b (S.Yield (Right p) c)) m =
  ([pure (updateProgress m p), liftJSM scroll, updateNext], m)
  where
    scroll = pure NoOp
    updateNext = pure (UpdateRestoreProgress b c)
getUpdate (UpdateRestoreProgress b (S.Yield (Left e) _)) m =
  ([handleError e], m)
  where
    handleError = \case
      API.ServerError s -> handleError' s
      API.InputError s -> handleError' s
    handleError' s =
      pure . SetModel $
        m & dialog . traverse . _ProgressDialog . _3
          .~ Model.ProcessError
            s
            ( Model.Function
                ( const . SetDialog . Just . Model.RestoreDialog $ b
                )
            )
getUpdate (UpdateRestoreProgress b (S.Effect c')) m = ([action], m)
  where
    action = liftIO $ c' >>= \c -> pure $ UpdateRestoreProgress b c
getUpdate (UpdateRestoreProgress b (S.Skip c)) m =
  getUpdate (UpdateRestoreProgress b c) m
getUpdate (UpdateRestoreProgress _ (S.Error e)) m =
  ([pure . SetDialog . Just $ ErrorDialog (RuntimeError e) m], m)
getUpdate (ToggleSelectedFile itemId) m = ([logToggle], newModel)
  where
    logToggle = do
      pure NoOp
    newModel =
      if HashSet.member itemId (m ^. selectedFiles)
        then m & selectedFiles .~ HashSet.delete itemId (m ^. selectedFiles)
        else m & selectedFiles .~ HashSet.insert itemId (m ^. selectedFiles)
getUpdate UpdateFiles m = ([doUpdate], m)
  where
    doUpdate = do
      log D "Updating local files"
      response <- liftJSM getFiles
      case response of
        Right fileList -> do
          let fileMap = HashMap.fromList $ zip [1 ..] fileList
          pure . SetFiles $ fileMap
        Left e -> pure . SetDialog . Just $ ErrorDialog e m
getUpdate UpdateBackups m = ([doUpdate], m)
  where
    doUpdate = do
      log D "Updating backups"
      response <- liftJSM getBackups
      case response of
        Right backupsList -> do
          let backupMap = HashMap.fromList $ zip [1 ..] backupsList
          pure . SetBackups $ backupMap
        Left e -> pure . SetDialog . Just $ ErrorDialog e m
getUpdate UpdateProjects m = ([doUpdate], m)
  where
    doUpdate = do
      log D "Updating projects"
      response <- liftJSM getProjects
      case response of
        Right projectsList -> do
          let projectMap = HashMap.fromList $ zip [1 ..] projectsList
          pure . SetProjects $ projectMap
        Left e -> pure . SetDialog . Just $ ErrorDialog e m
getUpdate UpdatePersons m = ([doUpdate], m)
  where
    doUpdate = do
      log D "Updating persons"
      response <- liftJSM getPersons
      case response of
        Right personsList -> do
          let personMap = HashMap.fromList $ zip [1 ..] personsList
          pure . SetPersons $ personMap
        Left e -> pure . SetDialog . Just $ ErrorDialog e m

updateProgress :: Model -> API.Progress -> Action
updateProgress model progress =
  SetModel $ model & dialog . traverse . _ProgressDialog . _2 %~ (progress :)

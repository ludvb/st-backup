{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | View
module STBackup.Web.View where

import Control.Lens
  ( Traversal',
    (&),
    (.~),
    (^.),
    (^..),
    (^?),
    _1,
    _2,
  )
import Control.Lens.Prism (_Just)
import Data.Char (toUpper)
import Data.Data.Lens (template)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
  ( fromMaybe,
    isJust,
  )
import Data.String (IsString)
import qualified Data.Text as T
import Data.Time
  ( defaultTimeLocale,
    formatTime,
  )
import Language.Javascript.JSaddle (toJSString)
import Miso
  ( View,
    button_,
    class_,
    defaultOptions,
    disabled_,
    div_,
    emptyDecoder,
    form_,
    h5_,
    href_,
    i_,
    id_,
    input_,
    label_,
    li_,
    link_,
    onClick,
    onInput,
    onWithOptions,
    option_,
    preventDefault,
    rel_,
    script_,
    select_,
    selected_,
    span_,
    src_,
    stringProp,
    style_,
    text,
    textarea_,
    type_,
    ul_,
    value_,
  )
import qualified Miso.Html.Element as HTML
import Miso.String
  ( FromMisoString,
    ToMisoString,
    fromMisoString,
    ms,
    unwords,
  )
import qualified STBackup.API as API
import qualified STBackup.API.Types.Field as Field
import qualified STBackup.Web.Model as Model
import Text.Printf (printf)
import Text.Read (readMaybe)
import Prelude hiding (unwords)

view :: Model.Model -> View Model.Action
view model = wrapView (defaultView model)

wrapView :: View Model.Action -> View Model.Action
wrapView x =
  div_
    []
    [ div_
        []
        [ script_
            [ src_ "https://cdn.jsdelivr.net/npm/jquery@3.6.0/dist/jquery.min.js",
              stringProp "crossorigin" "anonymous"
            ]
            "",
          script_
            [ src_
                "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/js/bootstrap.bundle.min.js",
              stringProp
                "integrity"
                "sha384-JEW9xMcG8R+pH31jmWH6WWP0WintQrMb4s7ZOdauHnUtxwoG2vI5DkLtS3qm9Ekf",
              stringProp "crossorigin" "anonymous"
            ]
            "",
          link_
            [ rel_ "stylesheet",
              href_
                "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/css/bootstrap.min.css"
            ],
          link_
            [ rel_ "stylesheet",
              href_
                "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.4.0/font/bootstrap-icons.css"
            ]
        ],
      x
    ]

symbolIcon :: Model.Symbol -> View action
symbolIcon s =
  i_
    [ class_ icon,
      style_ $
        M.fromList
          [ ("font-size", "2rem"),
            ("color", color),
            ("vertical-align", "middle"),
            ("margin-right", "1rem")
          ]
    ]
    []
  where
    icon = case s of
      Model.ErrorSymbol -> "bi-exclamation-octagon"
      Model.CheckMarkSymbol -> "bi-check-circle"
      Model.NoSymbol -> ""
    color = case s of
      Model.ErrorSymbol -> "red"
      Model.CheckMarkSymbol -> "green"
      Model.NoSymbol -> "black"

defaultView :: Model.Model -> View Model.Action
defaultView model@Model.Model {..} =
  div_ [style_ $ M.fromList [("padding", "0em 1em 1em 1em")]] $
    [showDialog x | x <- toList _dialog]
      <> [view' _state]
  where
    view' Model.Upload =
      div_ [] [viewNavbar model, viewFiles _files _selectedFiles]
    view' Model.Restore =
      div_ [] [viewNavbar model, viewBackups _backups _projects _persons]

    showDialog (Model.NotificationDialog Model.Notification {..}) =
      constructModal
        (Just (text "Close", Model.SetDialog Nothing))
        Nothing
        Nothing
        $ div_ [] [symbolIcon _notificationSymbol, text (ms _notificationMessage)]
    showDialog (Model.ErrorDialog e returnModel) =
      constructModal
        (Just (text "Close", Model.SetModel returnModel))
        Nothing
        (Just "Error")
        $ div_ [] [symbolIcon Model.ErrorSymbol, text . ms . show $ e]
    showDialog (Model.ProgressDialog name progressMessages processStatus) =
      constructModal
        ( case processStatus of
            Model.ProcessRunning -> Nothing
            Model.ProcessError _ a -> Just (text "Close", Model.unFunction a ())
            Model.ProcessDone a -> Just (text "Close", Model.unFunction a ())
        )
        Nothing
        (Just (toUpper (head prettyTitle) : tail prettyTitle <> "..."))
        $ div_
          []
          [ case processStatus of
              Model.ProcessError s _ -> showError s
              Model.ProcessRunning -> showProgress
              Model.ProcessDone _ -> showProgress,
            showLog
          ]
      where
        prettyTitle = case name of
          "backup" -> "backing up data"
          _ -> name
        showError s =
          div_
            []
            [ symbolIcon Model.ErrorSymbol,
              text . ms $ "Error: " <> s
            ]
        showProgress =
          case progressMessages of
            p : _ -> showProgress' p
            _ -> div_ [] []
          where
            showProgress' API.Progress {..} =
              div_
                []
                [ div_
                    [ class_ "progress",
                      style_ $ M.fromList [("width", "100%")]
                    ]
                    [ div_
                        [ class_ "progress-bar",
                          style_ $
                            M.fromList [("width", ms (_progressComplete * 100) <> "%")],
                          stringProp "role" "progressbar",
                          stringProp "aria-valuenow" (show (_progressComplete * 100)),
                          stringProp "aria-valuemin" "0",
                          stringProp "aria-valuemax" "100"
                        ]
                        [ text
                            (ms (printf "%.0f" (_progressComplete * 100) :: String) <> "%")
                        ]
                    ],
                  div_
                    [style_ $ M.fromList [("margin-top", "0.75rem")]]
                    [ span_
                        [ class_ $
                            if _progressComplete < 1.0
                              then unwords ["spinner-border", "spinner-border-sm"]
                              else unwords ["bi", "bi-check"],
                          style_ $ M.fromList [("margin-right", "0.5em")]
                        ]
                        [],
                      text (toJSString _progressStage)
                    ]
                ]
        showLog =
          div_
            [ id_ "msg-log",
              class_ "bg-light border",
              style_ $
                M.fromList
                  [ ("height", "20rem"),
                    ("resize", "vertical"),
                    ("overflow", "auto"),
                    ("width", "100%"),
                    ("font-size", "0.75rem"),
                    ("margin-top", "0.75rem"),
                    ("padding", "0.2rem"),
                    ("font-family", "monospace"),
                    ("overflow-wrap", "break-word")
                  ]
            ]
            $ [ div_
                  []
                  [ text . ms . showReceived $ _progressTime,
                    text "  ",
                    showHandle progressLog,
                    span_
                      [style_ $ M.fromList [("font-weight", "bold")]]
                      [text " > ", showLogText progressLog]
                  ]
                | API.Progress {..} <- take nProcessing progressMessages,
                  progressLog <- toList _progressLog
              ]
              <> [ div_
                     []
                     [ text . ms . showReceived $ _progressTime,
                       text "  ",
                       showHandle progressLog,
                       text " ",
                       showLogText progressLog
                     ]
                   | API.Progress {..} <- visibleMessages,
                     progressLog <- toList _progressLog
                 ]
              <> let nTruncated = length allMessages - length visibleMessages
                  in [ div_ [] [text ("(+" <> ms (show nTruncated) <> " messages not shown)")]
                       | nTruncated > 0
                     ]
          where
            nProcessing = case processStatus of
              Model.ProcessRunning -> 1
              _ -> 0
            maxMessages = case processStatus of
              Model.ProcessRunning -> Just 100
              _ -> Nothing
            allMessages =
              [ message
                | message@API.Progress {..} <- drop nProcessing progressMessages,
                  let logMessage =
                        _progressLog
                          ^. _Just . (template :: Traversal' API.LogMessage String)
                   in logMessage /= [] && '\r' /= last logMessage
              ]
            visibleMessages = case maxMessages of
              Just n -> take n allMessages
              Nothing -> allMessages
            showReceived = formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]"
            showHandle =
              \case
                API.LogInfo _ -> text ""
                API.LogOut _ ->
                  span_
                    []
                    [ text "(",
                      span_ [style_ $ M.fromList [("color", "var(--bs-blue)")]] [text "stdout"],
                      text ")"
                    ]
                API.LogErr _ ->
                  span_
                    []
                    [ text "(",
                      span_ [style_ $ M.fromList [("color", "var(--bs-yellow)")]] [text "stderr"],
                      text ")"
                    ]
            showLogText =
              \case
                API.LogInfo s -> text . ms . T.strip . T.pack $ s
                API.LogOut s -> text . ms . T.strip . T.pack $ s
                API.LogErr s -> text . ms . T.strip . T.pack $ s
    showDialog
      ( Model.BackupDialog
          backupRequest@API.BackupRequest {_backup = (files, _)}
        ) =
        constructModal
          (Just (text "Close", Model.SetDialog Nothing))
          (Just (text "Upload", Model.SubmitBackupDialog backupRequest))
          (Just "Upload backup data")
          $ div_
            []
            [ label_ [class_ "form-label"] [text "Project:"],
              validatedSelect
                model
                ( Model.dialog
                    . traverse
                    . Model._BackupDialog
                    . API.backup
                    . _2
                    . API.metadataProject
                )
                ( model ^.. Model.projects . traverse <&> \API.Project {..} ->
                    ( fromMaybe (-1) (Field.fromField _projectId),
                      Field.fromField _projectName
                    )
                )
                (-1)
                ( Model.SetDialog . Just $
                    Model.ProjectsDialog
                      API.Project
                        { _projectName = Field.toField "",
                          _projectId = Field.toField Nothing
                        }
                      model
                      ( Model.Function
                          ( \projectIdx ->
                              Model.SetModel $
                                model
                                  & Model.dialog
                                    . traverse
                                    . Model._BackupDialog
                                    . API.backup
                                    . _2
                                    . API.metadataProject
                                    .~ Field.toField projectIdx
                          )
                      )
                )
                hasErrors,
              label_
                [class_ "form-label"]
                [text "Contact person:"],
              validatedSelect
                model
                ( Model.dialog
                    . traverse
                    . Model._BackupDialog
                    . API.backup
                    . _2
                    . API.metadataContact
                )
                ( model ^.. Model.persons . traverse <&> \API.Person {..} ->
                    ( fromMaybe (-1) (Field.fromField _personId),
                      Field.fromField _personName
                    )
                )
                (-1)
                ( Model.SetDialog . Just $
                    Model.PersonsDialog
                      API.Person
                        { _personName = Field.toField "",
                          _personId = Field.toField Nothing
                        }
                      model
                      ( Model.Function
                          ( \personIdx ->
                              Model.SetModel $
                                model
                                  & Model.dialog
                                    . traverse
                                    . Model._BackupDialog
                                    . API.backup
                                    . _2
                                    . API.metadataContact
                                    .~ Field.toField personIdx
                          )
                      )
                )
                hasErrors,
              label_
                [class_ "form-label"]
                [text "Date of experiment:"],
              validatedInput
                model
                ( Model.dialog
                    . traverse
                    . Model._BackupDialog
                    . API.backup
                    . _2
                    . API.metadataExperimentDate
                )
                hasErrors,
              label_
                [class_ "form-label"]
                [text "Description (optional):"],
              validatedTexarea
                model
                ( Model.dialog
                    . traverse
                    . Model._BackupDialog
                    . API.backup
                    . _2
                    . API.metadataDescription
                )
                hasErrors,
              label_
                [class_ "form-label"]
                [text "Send backup confirmation e-mail to (optional):"],
              validatedInput
                model
                ( Model.dialog
                    . traverse
                    . Model._BackupDialog
                    . API.mail
                )
                hasErrors,
              div_
                [class_ "input-group", class_ "mb-3"]
                [ label_ [class_ "form-label"] [text "Files:"],
                  div_
                    [style_ $ M.fromList [("width", "100%")]]
                    [ ul_
                        [class_ "list-group"]
                        [ li_ [class_ "list-group-item fw-light"] [viewFileWithIcon file]
                          | file <- files
                        ]
                    ]
                ]
            ]
        where
          hasErrors =
            backupRequest
              ^.. (template :: Traversal' API.BackupRequest Field.ErrorS)
                . Field._HasError
              /= []
    showDialog (Model.RestoreDialog backup@(files, API.Metadata {..})) =
      constructModal
        (Just (text "Close", Model.SetDialog Nothing))
        (Just (text "Restore", Model.SubmitRestoreDialog backup))
        ( HashMap.lookup (Field.fromField _metadataProject) _projects
            <&> API._projectName
            <&> Field.fromField
        )
        $ div_
          [class_ "d-grid gap-3"]
          [ item "Contact"
              . text
              . ms
              . fromMaybe "???"
              $ HashMap.lookup (Field.fromField _metadataContact) _persons
                <&> API._personName
                <&> Field.fromField,
            item "Experiment date" . text . ms . Field.fromField $
              _metadataExperimentDate,
            item "Backup date" . text . ms . fromMaybe "???"
              . Field.fromField
              $ _metadataBackupDate,
            item "Description" $
              div_
                [style_ $ M.fromList [("white-space", "pre-wrap")]]
                [text . ms . Field.fromField $ _metadataDescription],
            item "Files" $
              ul_
                [class_ "list-group"]
                [ li_ [class_ "list-group-item fw-light"] [viewFileWithIcon file]
                  | file <- files
                ]
          ]
      where
        item itemName itemContents =
          div_
            [class_ "p-2 bg-light border"]
            [label_ [] [text itemName], div_ [class_ "fw-light"] [itemContents]]
    showDialog (Model.PersonsDialog formData cancelModel returnFunction) =
      constructModal
        (Just (text "Cancel", Model.SetModel cancelModel))
        (Just (text "Submit", Model.SubmitPersonsDialog formData cancelModel returnFunction))
        (Just "Add new contact person")
        $ div_
          []
          [ label_ [class_ "form-label"] [text "Name:"],
            validatedInput
              model
              ( Model.dialog
                  . traverse
                  . Model._PersonsDialog
                  . _1
                  . API.personName
              )
              hasErrors
          ]
      where
        hasErrors =
          formData ^.. (template :: Traversal' API.Person Field.ErrorS) . Field._HasError
            /= []
    showDialog (Model.ProjectsDialog formData cancelModel returnFunction) =
      constructModal
        (Just (text "Cancel", Model.SetModel cancelModel))
        (Just (text "Submit", Model.SubmitProjectsDialog formData cancelModel returnFunction))
        (Just "Add new project")
        $ div_
          []
          [ label_ [class_ "form-label"] [text "Project name:"],
            validatedInput
              model
              ( Model.dialog
                  . traverse
                  . Model._ProjectsDialog
                  . _1
                  . API.projectName
              )
              hasErrors
          ]
      where
        hasErrors =
          formData ^.. (template :: Traversal' API.Project Field.ErrorS) . Field._HasError
            /= []

validatedInput ::
  (IsString a, ToMisoString a, FromMisoString a) =>
  Model.Model ->
  Traversal' Model.Model (Field.Field a) ->
  Bool ->
  View Model.Action
validatedInput model input showValidation =
  div_
    [class_ "input-group mb-3"]
    ( input_
        ( [ class_ "form-control",
            value_ . ms . fromMaybe "" $ model ^? input . Field.value,
            onInput $
              \x -> Model.SetModel $ model & input . Field.value .~ fromMisoString x
          ]
            ++ [class_ "is-valid" | showValidation]
            ++ [class_ "is-invalid" | showValidation, _ <- toList errorValue]
        ) :
        [ div_ [class_ "invalid-feedback"] [text . ms $ errorMessage]
          | errorMessage <- toList errorValue
        ]
    )
  where
    errorValue = model ^. input . Field.error

validatedTexarea ::
  (IsString a, ToMisoString a, FromMisoString a) =>
  Model.Model ->
  Traversal' Model.Model (Field.Field a) ->
  Bool ->
  View Model.Action
validatedTexarea model input showValidation =
  div_
    [class_ "input-group mb-3"]
    ( textarea_
        ( [ class_ "form-control",
            onInput $
              \x -> Model.SetModel $ model & input . Field.value .~ fromMisoString x
          ]
            ++ [class_ "is-valid" | showValidation]
            ++ [class_ "is-invalid" | showValidation, _ <- toList errorValue]
        )
        [text . ms . fromMaybe "" $ model ^? input . Field.value] :
        [ div_ [class_ "invalid-feedback"] [text . ms $ errorMessage]
          | errorMessage <- toList errorValue
        ]
    )
  where
    errorValue = model ^. input . Field.error

validatedSelect ::
  (Num a, Show a, Read a, Eq a) =>
  Model.Model ->
  Traversal' Model.Model (Field.Field a) ->
  [(a, String)] ->
  a ->
  Model.Action ->
  Bool ->
  View Model.Action
validatedSelect model input options defaultValue editAction showValidation =
  div_ [class_ "input-group mb-3"] $
    [ select_
        ( [ class_ "form-select",
            onInput $ \x ->
              Model.SetModel $
                model
                  & input
                    . Field.value
                  .~ (fromMaybe defaultValue . readMaybe . fromMisoString $ x)
          ]
            ++ [class_ "is-valid" | showValidation]
            ++ [class_ "is-invalid" | showValidation, _ <- toList errorValue]
        )
        ( option_ [] [] :
            [ option_
                [ value_ (ms . show $ optionId),
                  selected_
                    ( case model ^? input . Field.value of
                        Just x -> x == optionId
                        _ -> False
                    )
                ]
                [text . ms $ optionString]
              | (optionId, optionString) <- options
            ]
        ),
      span_
        [class_ "btn btn-secondary", onClick editAction]
        [i_ [class_ "bi bi-pencil-square"] []]
    ]
      ++ [ div_ [class_ "invalid-feedback"] [text . ms $ errorMessage]
           | errorMessage <- toList errorValue
         ]
  where
    errorValue = model ^. input . Field.error

constructModal ::
  Maybe (View Model.Action, Model.Action) ->
  Maybe (View Model.Action, Model.Action) ->
  Maybe String ->
  View Model.Action ->
  View Model.Action
constructModal closeButton submitButton title contents =
  div_
    []
    [ div_
        [ style_ $
            M.fromList
              [ ("position", "absolute"),
                ("width", "100%"),
                ("height", "100%"),
                ("left", "0"),
                ("top", "0"),
                ("z-index", "2")
              ]
        ]
        [ div_
            [ class_ "modal-dialog",
              style_ $ M.fromList [("opacity", "0.9")]
            ]
            [ container $
                [ header,
                  div_ [class_ "modal-body"] [contents]
                ]
                  <> [footer | isJust closeButton || isJust submitButton]
            ]
        ],
      div_
        [ style_ $
            M.fromList
              [ ("position", "fixed"),
                ("width", "100%"),
                ("height", "100%"),
                ("left", "0"),
                ("top", "0"),
                ("z-index", "1"),
                ("background", "rgba(0,0,0,0.2)"),
                ("backdrop-filter", "blur(2px)")
              ]
        ]
        []
    ]
  where
    container = case submitButton of
      Just (_, submitAction) ->
        form_
          [ class_ "modal-content needs-validation",
            onWithOptions
              defaultOptions {preventDefault = True}
              "submit"
              emptyDecoder
              (const submitAction)
          ]
      _ -> div_ [class_ "modal-content"]
    header =
      div_ [class_ "modal-header"] $
        [h5_ [class_ "modal-title"] [text (ms x)] | x <- toList title]
          <> [ button_
                 [ type_ "button",
                   class_ "btn-close",
                   stringProp "data-bs-dismiss" "modal",
                   stringProp "aria-label" "Close",
                   onClick closeAction
                 ]
                 []
               | (_, closeAction) <- toList closeButton
             ]
    footer =
      div_ [class_ "modal-footer"] $
        [ button_
            [ type_ "button",
              class_
                . unwords
                $ [ "btn",
                    if isJust submitButton
                      then "btn-secondary"
                      else "btn-primary"
                  ],
              onClick closeAction
            ]
            [closeText]
          | (closeText, closeAction) <- toList closeButton
        ]
          <> [ button_
                 [ type_ "submit",
                   class_ . unwords $ ["btn", "btn-primary"]
                 ]
                 [actionText]
               | (actionText, _) <- toList submitButton
             ]

viewNavbar :: Model.Model -> View Model.Action
viewNavbar Model.Model {..} =
  div_
    [class_ "navbar"]
    ( div_
        [class_ "nav", class_ "nav-tabs"]
        [ div_
            [class_ "nav-item"]
            [ button_
                ( [class_ "nav-link", onClick (Model.SetState Model.Upload)]
                    ++ [class_ "active" | _state == Model.Upload]
                )
                [text "Backup"]
            ],
          div_
            [class_ "nav-item"]
            [ button_
                ( [class_ "nav-link", onClick (Model.SetState Model.Restore)]
                    ++ [class_ "active" | _state == Model.Restore]
                )
                [text "Restore"]
            ]
        ] :
        [ button_
            [ class_ "btn",
              class_ "btn-primary",
              onClick
                (Model.RunActions [Model.UpdatePersons, Model.UpdateProjects, Model.SpawnBackupDialog]),
              disabled_ (HashSet.size _selectedFiles == 0)
            ]
            [text "Upload"]
          | _state == Model.Upload
        ]
    )

viewFiles :: HashMap.HashMap Integer API.File -> HashSet.HashSet Integer -> View Model.Action
viewFiles files selectedFiles =
  div_
    []
    [ HTML.style_ [] ".file:hover { background: var(--bs-light); }",
      div_
        [class_ "list-group"]
        [ div_
            [ class_ $
                unwords $
                  ["list-group-item", "file"]
                    <> ["bg-primary text-white" | HashSet.member fileId selectedFiles],
              onClick (Model.ToggleSelectedFile fileId),
              style_ $ M.fromList [("cursor", "pointer")]
            ]
            [ viewFileWithIcon file,
              div_
                [class_ "fw-light", class_ "fst-italic"]
                [text . ms $ formatTime defaultTimeLocale "%Y-%m-%d" _fileModified]
            ]
          | (fileId, file@API.File {..}) <-
              sortBy
                (\(_, API.File {_fileModified = a}) (_, API.File {_fileModified = b}) -> compare b a)
                (HashMap.toList files)
        ]
    ]

viewBackups ::
  HashMap.HashMap Integer ([API.File], API.Metadata) ->
  HashMap.HashMap Integer API.Project ->
  HashMap.HashMap Integer API.Person ->
  View Model.Action
viewBackups backups projects persons =
  div_
    []
    [ HTML.style_ [] ".file:hover { background: var(--bs-light); }",
      div_
        [class_ "list-group"]
        [ div_
            [ class_ $ unwords ["list-group-item", "file"],
              onClick (Model.SetDialog . Just $ Model.RestoreDialog backup),
              style_ $ M.fromList [("cursor", "pointer")]
            ]
            [ div_
                []
                [ text
                    . ms
                    . fromMaybe "???"
                    $ HashMap.lookup (Field.fromField _metadataProject) projects
                      <&> API._projectName
                      <&> Field.fromField
                ],
              div_
                [class_ "fw-light"]
                [ text . ms $
                    "Contact: "
                      <> fromMaybe
                        "???"
                        ( HashMap.lookup (Field.fromField _metadataContact) persons
                            <&> API._personName
                            <&> Field.fromField
                        )
                ],
              div_
                [class_ "fw-light"]
                [ text
                    . ms
                    $ "Date of experiment: "
                      <> Field.fromField _metadataExperimentDate
                ]
            ]
          | (_, backup@(_, API.Metadata {..})) <- HashMap.toList backups
        ]
    ]

viewFileWithIcon :: API.File -> View a
viewFileWithIcon API.File {..} =
  div_
    []
    [ i_
        [ class_ $ case _fileType of
            API.Archive -> "bi-file-earmark-zip"
            API.Binary -> "bi-file-binary"
            API.Directory -> "bi-folder"
            API.Image -> "bi-file-earmark-image"
            API.Text -> "bi-file-earmark-text"
            _ -> "bi-file-earmark",
          style_ $
            M.fromList
              [ ("vertical-align", "middle"),
                ("margin-right", "0.5rem")
              ]
        ]
        [],
      span_ [class_ "fw-light"] [text . ms $ _fileDir <> "/"],
      text . ms $ _fileName
    ]

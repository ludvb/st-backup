{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Applicative (Alternative (empty))
import Control.Lens ((%~), (&), (.~), _1, _2)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BSL
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (getCurrentTime)
import Network.Mail.Mime (Address (Address), addAttachmentsBS, simpleMail')
import Network.Mail.SMTP (sendMailTLS', sendMailWithLoginTLS')
import Polysemy (Embed, Member, Members, Sem, embed, interpret)
import Polysemy.Error (Error, throw)
import Polysemy.Fail (Fail (Fail))
import Polysemy.NonDet (runNonDetMaybe)
import Polysemy.Reader (Reader, ask, asks, runReader)
import Polysemy.Resource (Resource, bracket, finally, onException, resourceToIO)
import Polysemy.State (State, get, modify)
import qualified STBackup.API as API
import STBackup.Backend.FileLocker (FileLocker)
import qualified STBackup.Backend.FileLocker as FileLocker
import qualified STBackup.Config as Config
import STBackup.Logging.Effect (Log, LogMessage, log, pattern D, pattern I, pattern W)
import qualified Servant.Types.SourceT as S
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp
  ( createTempDirectory,
    getCanonicalTemporaryDirectory,
  )
import Prelude hiding (log)

runFailAsError ::
  forall r a b.
  (Member (Error b) r) =>
  (String -> b) ->
  Sem (Fail ': r) a ->
  Sem r a
runFailAsError f = do
  interpret $ \case Fail s -> throw (f s)

withTempDir ::
  Members '[Embed IO, Resource, Log LogMessage] r =>
  (FilePath -> Sem r a) ->
  Sem r a
withTempDir =
  bracket
    ( embed getCanonicalTemporaryDirectory
        >>= embed . flip createTempDirectory "st-backup"
    )
    ( \tempDir -> do
        log I $ "Removing " <> show tempDir
        embed $ removeDirectoryRecursive tempDir
    )

type ConsumeStepIO a b = S.StepT IO a -> IO b

type SourceIOEffect a b = Reader (ConsumeStepIO a b)

runSourceIOEffect ::
  Members '[Embed IO] r =>
  Sem (SourceIOEffect a b ': r) c ->
  ConsumeStepIO a b ->
  Sem r b
runSourceIOEffect program consume = do
  _ <- runReader consume program
  embed (consume S.Stop)

yield :: Members '[Embed IO, SourceIOEffect a b] r => a -> Sem r b
yield x = do
  consume <- ask
  embed $ consume (S.Yield x S.Stop)

yieldSafely :: Members '[Embed IO, SourceIOEffect a b] r => a -> Sem r (Maybe b)
yieldSafely x = runNonDetMaybe . resourceToIO $ onException (yield x) empty

yieldProgress ::
  ( Eq stage,
    Show stage,
    Members
      '[ Embed IO,
         Reader (ConsumeStepIO (Either a API.Progress) b),
         State (stage, [stage]),
         Log LogMessage
       ]
      r
  ) =>
  Maybe API.LogMessage ->
  Sem r ()
yieldProgress msg = do
  (curStage, stageList) <- get
  curTime <- embed getCurrentTime
  let stageIndex = fromMaybe 0 $ curStage `elemIndex` stageList
      progress =
        API.Progress
          { _progressComplete = fromIntegral stageIndex / fromIntegral (length stageList - 1),
            _progressStage = show curStage,
            _progressTime = curTime,
            _progressLog = msg
          }
  forM_ msg $
    log I . \case
      API.LogInfo s -> s
      API.LogOut s -> "<stdout> " <> s
      API.LogErr s -> "<stderr> " <> s
  _ <- yieldSafely (Right progress)
  pure ()

yieldProgressMessage ::
  ( Eq stage,
    Show stage,
    Members
      '[ Embed IO,
         Reader (ConsumeStepIO (Either a API.Progress) b),
         State (stage, [stage]),
         Log LogMessage
       ]
      r
  ) =>
  API.LogMessage ->
  Sem r ()
yieldProgressMessage msg =
  yieldProgress (Just msg)

setStage ::
  ( Eq stage,
    Show stage,
    Members
      '[ Embed IO,
         Reader (ConsumeStepIO (Either a API.Progress) b),
         State (stage, [stage]),
         Log LogMessage
       ]
      r
  ) =>
  stage ->
  Sem r ()
setStage newStage = do
  modify (& _1 .~ newStage)
  yieldProgress Nothing

withFileLocks ::
  Members '[FileLocker lock, Log LogMessage, Embed IO, Resource, Fail] r =>
  [FilePath] ->
  Sem r a ->
  Sem r a
withFileLocks [] successAction = successAction
withFileLocks (f : fs') successAction = do
  log D $ "Locking: " <> f
  maybeLock <- FileLocker.lock f
  case maybeLock of
    Just lockHandle -> do
      finally (withFileLocks fs' successAction) $ do
        log D $ "Unlocking: " <> f
        onException
          (FileLocker.release lockHandle)
          (log W "Failed to release file lock")
    Nothing -> do
      fail $ "File " <> show f <> " is locked"

sendMail ::
  Members
    '[Embed IO, Reader Config.ApiServerConfig, Log LogMessage]
    r =>
  String ->
  String ->
  String ->
  [(String, String, BSL.ByteString)] ->
  Sem r ()
sendMail to subject message attachments = do
  log I $ "Sending mail to " <> to <> " (subject: \"" <> subject <> "\")"
  Config.SmtpConfig {..} <- asks Config._apiServerSmtpConfig
  let attachments' =
        attachments
          & (traverse . _1 %~ T.pack)
          & (traverse . _2 %~ T.pack)
      mail =
        simpleMail'
          (Address Nothing (T.pack to))
          (Address (Just (T.pack _smtpSenderName)) (T.pack _smtpSenderAddress))
          (T.pack subject)
          (TL.pack message)
          & addAttachmentsBS attachments'
  let sendMail' = case sequence [_smtpUsername, _smtpPassword] of
        Just [user, password] -> \host port mail' ->
          sendMailWithLoginTLS'
            host
            port
            user
            password
            mail'
        _ -> sendMailTLS'
   in embed $ sendMail' _smtpHost (fromIntegral _smtpPort) mail

module STBackup.Common where

import Control.Concurrent (threadDelay)
import qualified Control.Exception (try)
import Data.Foldable (Foldable (toList))
import Polysemy (Embed, Members, Sem, embed)
import Polysemy.Fail (Fail)
import Polysemy.Reader (Reader, asks)
import qualified STBackup.API as API
import qualified STBackup.Config as Config
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, hGetChar, hReady)
import System.Process
  ( CmdSpec (RawCommand, ShellCommand),
    CreateProcess (std_err, std_out),
    StdStream (CreatePipe),
    cmdspec,
    createProcess,
    proc,
    waitForProcess,
  )
import Prelude hiding (log)

hGetUntil :: (Char -> Bool) -> Handle -> IO String
hGetUntil predicate h = do
  c <- hGetChar h
  if predicate c then pure [c] else (c :) <$> hGetUntil predicate h

execute ::
  Members '[Embed IO, Fail] r =>
  CreateProcess ->
  (API.LogMessage -> Sem r ()) ->
  Sem r ()
execute p f = do
  _ <- f . API.LogInfo $ "Executing: " <> show p

  (_, Just hout, Just herr, ph) <-
    embed $
      createProcess
        p
          { std_out = CreatePipe,
            std_err = CreatePipe
          }

  let consumeOutput = do
        eitherOutReady <- embed $ Control.Exception.try $ hReady hout
        eitherErrReady <- embed $ Control.Exception.try $ hReady herr
        case (eitherOutReady, eitherErrReady) of
          (Left (_ :: IOError), Left (_ :: IOError)) -> pure ()
          (Right True, _) -> embed (hGetUntil (`elem` "\n\r") hout) >>= f . API.LogOut >> consumeOutput
          (_, Right True) -> embed (hGetUntil (`elem` "\n\r") herr) >>= f . API.LogErr >> consumeOutput
          _ -> embed (threadDelay (100 * 1000)) >> consumeOutput
   in consumeOutput

  exitCode <- embed $ waitForProcess ph
  let commandName = case cmdspec p of
        ShellCommand s -> s
        RawCommand fp _args -> fp
      exitMessage = \c -> "Program " <> show commandName <> " exited with code " <> show c
   in case exitCode of
        ExitSuccess -> pure ()
        ExitFailure c -> fail (exitMessage c)

encrypt ::
  Members '[Embed IO, Fail, Reader Config.ApiServerConfig] r =>
  FilePath ->
  FilePath ->
  (API.LogMessage -> Sem r ()) ->
  Sem r ()
encrypt src dst f = do
  gpgReceiver <- asks Config._apiServerGPGReceiver
  _ <- execute (proc "gpg" ["--batch", "-ver", gpgReceiver, "-o", dst, src]) f
  pure ()

decrypt ::
  Members '[Embed IO, Fail, Reader Config.ApiServerConfig] r =>
  FilePath ->
  FilePath ->
  (API.LogMessage -> Sem r ()) ->
  Sem r ()
decrypt src dst f = do
  gpgReceiver <- asks Config._apiServerGPGReceiver
  gpgPassphraseFile <- asks Config._apiServerGPGPassphraseFile
  _ <-
    execute
      ( proc
          "gpg"
          ( ["--batch", "-vdr", gpgReceiver]
              <> ["--passphrase-file=" <> x | x <- toList gpgPassphraseFile]
              <> ["-o", dst, src]
          )
      )
      f
  pure ()

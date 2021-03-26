{-# LANGUAGE OverloadedStrings #-}

-- | Effect for naive file locking by creating empty ".lock" files.
--
-- TODO: This is a hack. Other processes will not understand the files are
-- locked.
--
-- /Background/: 'GHC.IO.FD.openFile' does not allow opening file descriptors on
-- directories. As a consequence, locking directories is not straightforward. We
-- should look for better ways.
module STBackup.Backend.FileLocker.Naive where

import Control.Lens ((%~))
import Control.Monad (guard)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Function ((&))
import Polysemy (Embed, Members, embed, interpret)
import STBackup.Backend.FileLocker
  ( FileLocker (..),
    FileLockerInterpreter (FileLockerInterpreter),
  )
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeFileName)
import System.FilePath.Lens (basename, extension)
import System.IO
  ( IOMode (WriteMode),
    hPrint,
    withFile,
  )
import System.Posix (getProcessID)
import Text.Regex.TDFA ((=~))
import Prelude hiding (log)

interpretNaiveFileLocker ::
  Members '[Embed IO] r =>
  FileLockerInterpreter FilePath r
interpretNaiveFileLocker = FileLockerInterpreter $
  interpret $ \case
    Lock filename -> runMaybeT $ do
      let lockFilename = lockFile filename
      exists <- liftIO $ doesFileExist lockFilename
      guard (not exists)
      liftIO $
        withFile lockFilename WriteMode $ \handle -> do
          pid <- getProcessID
          hPrint handle pid
          pure lockFilename
    Release lockFilename -> do
      pid <- embed getProcessID
      lockFilePid <- embed $ readFile lockFilename
      if pid == read lockFilePid
        then embed $ removeFile lockFilename
        else pure ()
    IsLocked filename ->
      embed $ doesFileExist (lockFile filename)

lockFile :: FilePath -> FilePath
lockFile filename =
  filename
    & basename %~ ("." <>)
    & extension %~ (<> ".lock")

isLockFile :: FilePath -> Bool
isLockFile = (=~ ("^\\..+\\.lock$" :: String)) . takeFileName

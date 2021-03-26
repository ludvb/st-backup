module Files where

import Control.Exception (try)
import Control.Monad (forM, guard)
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Polysemy (Embed, Members, Sem, embed)
import qualified STBackup.API as API
import STBackup.Backend.FileLocker (FileLocker, isLocked)
import System.Directory (getModificationTime, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.Process (readProcess)
import Text.Regex.TDFA ((=~))

getLocalFiles ::
  Members '[Embed IO, FileLocker lock] r =>
  String ->
  FilePath ->
  Sem r [API.File]
getLocalFiles fileDirName fileDir = do
  systemFiles <- embed $ listDirectory fileDir <&> fmap (fileDir </>)
  maybeFiles <- forM systemFiles $ \file -> runMaybeT $ do
    isUnlocked <- lift $ isLocked file

    guard (not (takeFileName file =~ ("^\\." :: String)))
    guard (not isUnlocked)

    -- Get modification time
    mtime <- liftIO $ getModificationTime file

    -- Get file type
    fileOutput :: Either IOError String <- liftIO $ try $ readProcess "file" [file] ""
    let fileType = case fileOutput of
          Right stdout | (stdout :: String) =~ (".*ASCII.*" :: String) -> API.Text
          Right stdout | (stdout :: String) =~ (".*compressed.*" :: String) -> API.Archive
          Right stdout | (stdout :: String) =~ (".*directory.*" :: String) -> API.Directory
          Right stdout | (stdout :: String) =~ (".*ELF.*" :: String) -> API.Binary
          Right stdout | (stdout :: String) =~ (".*image.*" :: String) -> API.Image
          _ -> API.Other

    pure $
      API.File
        { API._fileName = takeFileName file,
          API._fileDir = fileDirName,
          API._fileModified = mtime,
          API._fileType = fileType
        }
  let files = catMaybes maybeFiles
   in pure files

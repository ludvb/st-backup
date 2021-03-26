{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STBackup.Logging.Core where

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle (JSString, jsg, toJSString, toJSVal, (#))
#endif

import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans (MonadIO (..))
import Data.Function ((&))
import Data.List
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, UTCTime)
import qualified System.Console.ANSI as ANSI
import System.IO
  ( Handle,
    IOMode (AppendMode),
    hClose,
    hPutStrLn,
    openFile,
    stderr,
  )

data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Read, Eq, Ord, Enum)

pattern D, I, W, E :: Severity
pattern D <- Debug where D = Debug
pattern I <- Info where I = Info
pattern W <- Warning where W = Warning
pattern E <- Error where E = Error

{-# COMPLETE D, I, W, E #-}

newtype LogAction m msg = LogAction
  { unLogAction :: msg -> m ()
  }

data LogMessage = LogMessage
  { msgSeverity :: !Severity,
    msgText :: !String,
    msgTime :: UTCTime
  } deriving Show

formatMessage :: MonadIO m => LogMessage -> m String
formatMessage LogMessage {..} = do
  let timeString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" msgTime
  pure $
    intercalate
      "  "
      [ "[ " <> fromString timeString <> " UTC ]",
        severityToText msgSeverity,
        T.unpack . T.strip . T.pack $ msgText
      ]
  where
    severityToText D = "DEBUG  " & withSGR [sgrFaint]
    severityToText I = "INFO   " & withSGR [sgrColor ANSI.Blue]
    severityToText W = "WARNING" & withSGR [sgrBold, sgrColor ANSI.Yellow]
    severityToText E = "ERROR  " & withSGR [sgrBold, sgrColor ANSI.Red]

    sgrColor = ANSI.SetColor ANSI.Foreground ANSI.Vivid
    sgrBold = ANSI.SetConsoleIntensity ANSI.BoldIntensity
    sgrFaint = ANSI.SetConsoleIntensity ANSI.FaintIntensity

    withSGR :: (IsString s, Semigroup s) => [ANSI.SGR] -> s -> s
    withSGR sgr x =
      let setCode = fromString . ANSI.setSGRCode
       in setCode sgr <> x <> setCode [ANSI.Reset]

logToHandle :: MonadIO m => Handle -> LogAction m LogMessage
logToHandle handle = LogAction $ \msg -> do
  formattedMsg <- formatMessage msg
  liftIO . hPutStrLn handle $ formattedMsg

logToFile :: (MonadIO m, MonadMask m) => FilePath -> LogAction m LogMessage
logToFile filePath = LogAction $ \msg ->
  bracket (liftIO $ openFile filePath AppendMode) (liftIO . hClose) $ \handle -> do
    unLogAction (logToHandle handle) msg

defaultLogAction :: MonadIO m => LogAction m LogMessage

#ifdef ghcjs_HOST_OS
defaultLogAction = LogAction $ \msg -> do
  formattedMsg <- liftIO $ toJSVal <$> formatMessage msg
  _ <- liftIO $ jsg ("console" :: JSString) # ("log" :: JSString) $ formattedMsg
  pure ()
#else
defaultLogAction = logToHandle stderr
#endif

{-# LANGUAGE OverloadedStrings #-}

-- | Effectful logging primitives
module STBackup.Logging.Effect
  ( module STBackup.Logging.Core,
    Log (..),
    log,
    emit,
    injectLog,
    injectLogId,
    unLogInjector,
    munchLog,
    traceLog,
  )
where

import Data.Coerce (coerce)
import Data.String (IsString)
import Polysemy (Embed, Member, Members, Sem, embed, intercept, interpret, makeSem, raise)
import Polysemy.Internal (send)
import Polysemy.State (modify, runState, get, modify')
import STBackup.Logging.Core
  ( LogAction (..),
    LogMessage (..),
    Severity (..),
    defaultLogAction,
    pattern D,
    pattern E,
    pattern I,
    pattern W,
  )
import Prelude hiding (log)
import Data.Time (getCurrentTime)

data Log msg t a where
  Emit :: msg -> Log msg t ()

makeSem ''Log

log :: Members '[Log LogMessage, Embed IO] r => Severity -> String -> Sem r ()
log sev = log'
  where
    log' text = do
      time <- embed $ getCurrentTime
      emit LogMessage {msgSeverity = sev, msgText = text, msgTime = time}

-- | Pops off 'Log' effects without action. Instead of using
-- 'Polysemy.interpret' as a single effect handler, multiple log handlers can be
-- injected with 'injectLog'. The 'munchLog' handler is placed at the bottom of
-- the handler stack to remove the log effect once it has been passed through
-- the handlers.
munchLog :: Sem (Log msg ': r) a -> Sem r a
munchLog = interpret $ \case Emit _ -> pure ()

newtype LogInjector msg r = LogInjector
  { unLogInjector ::
      forall a.
      Member (Log msg) r =>
      Sem r a ->
      Sem r a
  }

injectLog ::
  Members '[Log msg, Embed m] r => LogAction m msg -> LogInjector msg r
injectLog (logAction :: LogAction m msg) =
  LogInjector $
    intercept @(Log msg) $
      \case
        x@(Emit msg) -> do
          embed $ unLogAction logAction msg
          send @(Log msg) (coerce x)

injectLogId ::
  Member (Log LogMessage) r => String -> Sem r a -> Sem r a
injectLogId identifier =
  intercept @(Log LogMessage) $
    \case
      Emit msg@LogMessage {..} ->
        send $ Emit msg {msgText = "(" <> identifier <> ") " <> msgText}

traceLog :: forall msg r a. (Members '[Log msg, Embed IO] r, Show msg) => Sem r a -> Sem r ([msg], a)
traceLog c = do
  runState [] . interceptor . raise $ c
  where
    interceptor = intercept @(Log msg) $
      \case
        Emit msg -> do
          modify' (msg :)
          send $ Emit msg

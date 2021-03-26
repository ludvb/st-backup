-- | Type class for file locking
module STBackup.Backend.FileLocker where

import Polysemy (Sem, makeSem)

data FileLocker lock m a where
  Lock :: FilePath -> FileLocker lock m (Maybe lock)
  Release :: lock -> FileLocker lock m ()
  IsLocked :: FilePath -> FileLocker lock m Bool

makeSem ''FileLocker

newtype FileLockerInterpreter lock r = FileLockerInterpreter
  { unFileLockerInterpreter ::
      forall a.
      Sem (FileLocker lock ': r) a ->
      Sem r a
  }

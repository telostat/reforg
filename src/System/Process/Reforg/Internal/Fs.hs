module System.Process.Reforg.Internal.Fs where

import Control.Monad.IO.Class (MonadIO)
import Path                   (Abs, Dir, File, Path)
import Path.IO                (listDir)


-- | List all files in a given directory.
listFiles :: MonadIO m => Path Abs Dir -> m [Path Abs File]
listFiles = fmap snd . listDir


-- | List and filters all files in a given directory.
filterFiles :: MonadIO m => (Path Abs File -> Bool) -> Path Abs Dir -> m [Path Abs File]
filterFiles p = fmap (filter p . snd) . listDir

module System.Process.Reforg where

import qualified Colog                                as L
import           Control.Monad.Except                 (throwError)
import           Path                                 (Abs, Dir, Path)
import           System.Process.Reforg.Internal.Class (Reforg, ReforgError(..))


-- | Reforg core program entry-point.
program :: [Path Abs Dir] -> Reforg ()
program _ = do
  L.logInfo "Starting..."
  _ <- throwError (ReforgErrorUnknown "Dummy testing error!")
  L.logInfo "Finished"

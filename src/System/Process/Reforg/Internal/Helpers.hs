module System.Process.Reforg.Internal.Helpers where

import qualified Data.Text                                 as T
import           System.Process.Reforg.Internal.Templating (mkTemplate)
import           System.Process.Reforg.Internal.Types      (KV(..))


-- | Attempts to create a 'KV' value from a given 'T.Text' valued key-value pair.
mkKeyedTemplate :: MonadFail m => (T.Text, T.Text) -> m KV
mkKeyedTemplate (k, v) = KV k <$> mkTemplate (T.unpack v)


-- | Attempts to create a list of 'KV's from a given list of 'T.Text' valued vkey-value pairs.
mkKeyedTemplates :: MonadFail m => [(T.Text, T.Text)] -> m [KV]
mkKeyedTemplates = traverse mkKeyedTemplate

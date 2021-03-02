module System.Process.Reforg.Internal.Types where

import           Data.Aeson ((.!=), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Text  as T


-- | Reforg process specification definition.
data Spec = Spec
  { specName   :: !T.Text
  , specIgnore :: ![String]
  } deriving (Show)


instance Aeson.FromJSON Spec where
  parseJSON = Aeson.withObject "Spec" $ \v -> Spec
    <$> v .:? "name"   .!= "Reforg Process Specification"
    <*> v .:? "ignore" .!= []

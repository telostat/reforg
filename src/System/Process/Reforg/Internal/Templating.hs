{-# LANGUAGE FlexibleContexts #-}

module System.Process.Reforg.Internal.Templating where

import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Writer   (Writer)
import qualified Data.Aeson             as Aeson
import qualified Data.Text              as T
import qualified Text.Ginger            as G


-- | Newtype definition for Ginger templates.
newtype Template = MkTemplate { unTemplate :: G.Template G.SourcePos } deriving Show


-- | 'Aeson.FromJSON' instance for 'Template'.
instance Aeson.FromJSON Template where
  parseJSON = Aeson.withText "Template" (mkTemplate . T.unpack)


-- | Smart constructor for 'Template' that operates in 'MonadFail' context.
--
-- >>> mkTemplate "" :: Maybe Template
-- Just (MkTemplate {unTemplate = Template {templateBody = NullS "<<unknown>>" (line 1, column 1), templateBlocks = fromList [], templateParent = Nothing}})
-- >>> mkTemplate "{{ a }}" :: Maybe Template
-- Just (MkTemplate {unTemplate = Template {templateBody = InterpolationS "<<unknown>>" (line 1, column 1) (VarE "<<unknown>>" (line 1, column 4) "a"), templateBlocks = fromList [], templateParent = Nothing}})
-- >>> mkTemplate "{{" :: Maybe Template
-- Nothing
mkTemplate :: MonadFail m => String -> m Template
mkTemplate s = either (fail . (<>) "Template parsing error: " . show) (pure . MkTemplate) parse
  where
    parse = runIdentity $ G.parseGinger (const $ return Nothing) Nothing s


-- | Renders a template with the given context.
--
-- >>> import qualified Data.HashMap.Strict as HM
-- >>> let template = mkTemplate "Value of a is '{{ a }}'" :: Maybe Template
-- >>> let context = HM.fromList [("a", "A")] :: HM.HashMap T.Text T.Text
-- >>> flip renderTemplate context <$> template
-- Just "Value of a is 'A'"
-- >>> let context = HM.fromList [("b", "B")] :: HM.HashMap T.Text T.Text
-- >>> flip renderTemplate context <$> template
-- Just "Value of a is ''"
renderTemplate :: G.ToGVal (G.Run G.SourcePos (Writer T.Text) T.Text) v => Template -> v -> T.Text
renderTemplate = flip G.easyRender . unTemplate

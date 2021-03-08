module System.Process.Reforg.Internal.Contextual where

import qualified Colog                                     as C
import qualified Data.HashMap.Strict                       as HM
import qualified Data.Text                                 as T
import           Path                                      (Abs, File, Path, fileExtension, filename, toFilePath)
import           System.Process.Reforg.Internal.Class      (Env(..), Reforg)
import           System.Process.Reforg.Internal.Templating (Template, renderTemplate)
import           System.Process.Reforg.Internal.Types      (Rule(..), Spec(..))


-- | Default environment.
defaultEnv :: Env Reforg
defaultEnv = Env
  { envEnvars = HM.empty
  , envParams = HM.empty
  , envRevars = HM.empty
  , envFsvars = HM.empty
  , envDryrun = False
  , envLogAction = C.richMessageAction
  }


-- | Compiles the environment to template render context.
compileContext :: Env Reforg -> HM.HashMap T.Text (HM.HashMap T.Text T.Text)
compileContext env = HM.fromList
  [ ("env", envEnvars env)
  , ("param", envParams env)
  , ("re", envRevars env)
  , ("file", envFsvars env)
  ]


-- | Updates the environment as per specification.
updateSpecEnv :: Spec -> Env Reforg -> Env Reforg
updateSpecEnv s = updateParams (specParams s) . updateEnvars (specEnvars s)


-- | Updates the environment as per file.
updateFileEnv :: Path Abs File -> Env Reforg -> Env Reforg
updateFileEnv = updateFsvars


-- | Updates the environment as per rule
updateRuleEnv :: Rule -> Env Reforg -> Env Reforg
updateRuleEnv r = updateParams (ruleParams r) . updateEnvars (ruleEnvars r)


-- | Updates the environment as per regular expression match
updateMatchEnv :: HM.HashMap T.Text T.Text -> Env Reforg -> Env Reforg
updateMatchEnv = updateRevars


-- | Updates environment with given environment variable templates.
updateEnvars :: HM.HashMap T.Text Template -> Env Reforg -> Env Reforg
updateEnvars ts e = e { envEnvars = HM.union (renderTemplates e ts) (envEnvars e) }


-- | Updates environment with given parameter templates.
updateParams :: HM.HashMap T.Text Template -> Env Reforg -> Env Reforg
updateParams ts e = e { envParams = HM.union (renderTemplates e ts) (envParams e) }


-- | Updates environment with given regular expression match groups.
updateRevars :: HM.HashMap T.Text T.Text -> Env Reforg -> Env Reforg
updateRevars rs e = e { envRevars = HM.union rs (envRevars e) }


-- | Updates environment with given path to file.
updateFsvars :: Path Abs File -> Env Reforg -> Env Reforg
updateFsvars p e = e
  { envFsvars = HM.fromList
      [ ("path", T.pack $ toFilePath p)
      , ("name", T.pack $ toFilePath $ filename p)
      , ("ext", maybe "" T.pack $ either (const Nothing) Just $ fileExtension p)
      ]
  }


-- | Updates environment with given dry-run flag.
updateDryrun :: Bool -> Env Reforg -> Env Reforg
updateDryrun b env = env { envDryrun = b }


-- | Helper function to render variable templates.
renderTemplates :: Functor f => Env Reforg -> f Template -> f T.Text
renderTemplates env = fmap (`renderTemplate` compileContext env)

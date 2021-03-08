-- | 'Reforg' core types module.

module System.Process.Reforg.Internal.Types where

import           Data.Aeson                                ((.!=), (.:), (.:?))
import qualified Data.Aeson                                as Aeson
import qualified Data.HashMap.Strict                       as HM
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Text                                 as T
import           System.Process.Reforg.Internal.Regex      (Regex)
import           System.Process.Reforg.Internal.Templating (Template)


-- | Reforg top-level specification definition.
data Spec = Spec
  { -- | Top-level title in plain text format for documentation purposes.
    specTitle       :: !T.Text
    -- | Top-level, optional description in Markdown format for documentation
    -- purposes.
  , specDescription :: !(Maybe T.Text)
    -- | Templated, key-value map of parameters that override parameters
    -- provided on the command-line.
  , specParams      :: !(HM.HashMap T.Text Template)
    -- | Templated, key-value map of environment variables that override
    -- environment variables found on the Reforg process environment and
    -- provided on the command line.
  , specEnvars      :: !(HM.HashMap T.Text Template)
    -- | Non-empty list of rules that will be tried for each file traversed.
  , specRules       :: !(NE.NonEmpty Rule)
    -- | Templated list of regular expressions to filter-out files to be
    -- processed.
  , specIgnore      :: ![Regex]
  } deriving (Show)


-- | 'Aeson.FromJSON' instance for 'Spec'.
instance Aeson.FromJSON Spec where
  parseJSON = Aeson.withObject "Spec" $ \v -> Spec
    <$> v .:? "title" .!= "Reforg Process Specification"
    <*> v .:? "description"
    <*> v .:? "params" .!= HM.empty
    <*> v .:? "envars" .!= HM.empty
    <*> v .:  "rules"
    <*> v .:? "ignore" .!= []


-- | A rule what will attempt to process a given file if the regular expression
-- matches the file name.
data Rule = Rule
  { -- | Title of the rule in plain text format for documentation purposes.
    ruleTitle       :: !T.Text
    -- | Optional description of the rule in Markdown format for documentation
    -- purposes.
  , ruleDescription :: !(Maybe T.Text)
    -- | Templated, key-value map of parameters that override parameters
    -- provided on the command-line and top-level specification parameters.
  , ruleParams      :: !(HM.HashMap T.Text Template)
    -- | Templated, key-value map of environment variables that override
    -- environment variables found on the Reforg process environment, provided
    -- on the command line and top-level specification.
  , ruleEnvars      :: !(HM.HashMap T.Text Template)
    -- | Templated regular expression that matches the filename of the current
    -- file attempted.
    --
    -- The syntax for the regular expressions are Perl-compatible. All
    -- named-groups will be available to the rest of the rule definition via
    -- @re.@ namespace.
  , ruleRegex       :: !(Maybe Template)
    -- | Non-empty list of 'Command' definitions to be attempted as per the
    -- attempted file if regular expression matches the filename.
  , ruleProcess     :: !(NE.NonEmpty Command)
    -- | Optional conditional (see 'When') to decide whether the attempted file
    -- should be processed.
  , ruleWhen        :: !(Maybe When)
  } deriving (Show)


-- | 'Aeson.FromJSON' instance for 'Rule'.
instance Aeson.FromJSON Rule where
  parseJSON = Aeson.withObject "Rule" $ \v -> Rule
    <$> v .:? "title" .!= "Rule"
    <*> v .:? "description"
    <*> v .:? "params" .!= HM.empty
    <*> v .:? "envars" .!= HM.empty
    <*> v .:? "re"
    <*> v .:  "process"
    <*> v .:? "when"


-- | Templated conditional definition as a sum-type of @sh@, @bash@ or @python@
-- expression that is supposed to return @True@ (`0` as exit value) or @False@
-- (Non-`0` as exit value).
data When =
    -- | Templated @sh@ expression/script such as:
    --
    -- > [ ! -f "/path/to/a/file"]
    WhenSh Template
    -- | Templated @bash@ expression/script such as:
    --
    -- > [ ! -f "/path/to/a/file"]
  | WhenBash Template
    -- | Templated @python@ expression/script such as:
    --
    -- > import sys
    -- > sys.exit(1)
  | WhenPython Template
  deriving (Show)


-- | 'Aeson.FromJSON' instance for 'When'.
instance Aeson.FromJSON When where
  parseJSON = Aeson.withObject "When" $ \v -> do
    sh <- v .:? "sh"
    bash <- v .:? "bash"
    python <- v .:? "python"
    case (sh, bash, python) of
      (Just c, _, _)             -> pure $ WhenSh c
      (Nothing, Just c, _)       -> pure $ WhenBash c
      (Nothing, Nothing, Just c) -> pure $ WhenPython c
      _                          -> fail "Unknown conditional interpreter for when."


-- | A command to run as a part of a 'Rule'\'s 'Process'.
data Command =
  Exec
    { -- | Title of the command in plain text format for documentation purposes.
      execTitle       :: !T.Text
      -- | Optional description of the command in Markdown format for
      -- documentation purposes.
    , execDescription :: !(Maybe T.Text)
    -- | Templated, key-value map of parameters that override parameters provided on the
    -- command-line, top-level specification parameters and parameters of the
    -- rule of process this command belongs to.
    , execParams      :: !(HM.HashMap T.Text Template)
    -- | Templated, key-value map of environment variables that override environment
    -- variables found on the Reforg process environment, provided on the
    -- command line, top-level specification and rule of process this command
    -- belongs to.
    , execEnvars      :: !(HM.HashMap T.Text Template)
    -- | Indication if the command accepts STDIN stream as provided from the
    -- previous process command's STDOUT.
    , execStdin       :: !Bool
    -- | Indication if the command writes to STDOUT stream that will be consumed
    -- by the next process command's STDIN.
    , execStdout      :: !Bool
    -- | Templated name of or path to the executable of the command.
    , execExecutable  :: !Template
    -- | Templated arguments to be passed to the command executable.
    , execArguments   :: ![Template]
    } deriving (Show)


-- | 'Aeson.FromJSON' instance for 'Command'.
instance Aeson.FromJSON Command where
  parseJSON = Aeson.withObject "Command" $ \v -> do
    cmd <- v .:? "command" .!= "exec"
    case cmd of
      "exec" -> makeExec v
      _      -> fail $ "Invalid command: " <> cmd
    where
      makeExec = \v -> Exec
        <$> v .:? "title" .!= "Command"
        <*> v .:? "description"
        <*> v .:? "params" .!= HM.empty
        <*> v .:? "envars" .!= HM.empty
        <*> v .:? "stdin" .!= True
        <*> v .:? "stdout" .!= True
        <*> v .:  "executable"
        <*> v .:? "arguments" .!= []

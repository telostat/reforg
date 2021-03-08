module System.Process.Reforg where

import qualified Colog                                     as C
import qualified Colog                                     as L
import           Control.Monad.Except                      (throwError)
import           Control.Monad.Reader                      (asks, local)
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Text                                 as T
import           Path                                      (Abs, Dir, File, Path, filename, toFilePath)
import           System.Process.Reforg.Internal.Class      (Reforg, ReforgError(..))
import           System.Process.Reforg.Internal.Contextual
                 ( compileContext
                 , updateFileEnv
                 , updateMatchEnv
                 , updateRuleEnv
                 , updateSpecEnv
                 )
import           System.Process.Reforg.Internal.Fs         (filterFiles)
import           System.Process.Reforg.Internal.Regex      (Regex, isMatch, match, mkRegex)
import           System.Process.Reforg.Internal.Templating (Template, renderTemplate)
import           System.Process.Reforg.Internal.Types      (Rule(..), Spec(..))
import           System.Process.Reforg.Internal.Utils      (anyM, concatMapM, ifM)


-- | Reforg core program entry-point.
program :: Spec -> [Path Abs Dir] -> Reforg ()
program s ds = do
  L.logInfo "Starting..."
  fs <- concatMapM (filterFiles (isIncluded (specIgnore s))) ds
  _ <- local (updateSpecEnv s) $ mapM_ go fs
  L.logInfo "Finished"
  where
    go f = local (updateFileEnv f) $ processFile f (NE.toList $ specRules s)


-- | Attempts to process the given file as per given rules.
processFile :: Path Abs File -> [Rule] -> Reforg ()
processFile f rs =
  ifM (anyM (processFileWithRule f) rs)
    (pure ())
    (throwError $ ReforgErrorNoMatch f)


-- | Attempts to process the file as per given rule.
--
-- If rule applies to the file, the function processes the file and returns
-- 'True'. If the rule does not apply to the file, the function simply returns
-- 'False'. Any errors during processing the file will be thrown in the
-- 'MonadError ReforgError' context. The call-site should decide on what to do
-- (retry, skip, propagate error etc...).
--
-- There are two main cases:
--
-- 1. The rule does NOT have a regular expression guard, ie. it applies to all files.
-- 2. The rule HAS a regular expression guard.
processFileWithRule :: Path Abs File -> Rule -> Reforg Bool
processFileWithRule f r = case ruleRegex r of
  Nothing -> local (updateRuleEnv r) $ runFile r f >> pure True
  Just retpl -> do
    re <- compileRegex retpl
    case match re (toFilePath $ filename f) of
      Nothing  -> pure False
      Just gvs -> local (updateRuleEnv r . updateMatchEnv gvs) $ runFile r f >> pure True


-- | Runs the file as per matched rule.
runFile :: Rule -> Path Abs File -> Reforg ()
runFile _ f = C.logInfo $ T.pack $ "Running file: " <> show f


-- | Helper function that attempts to compile the regular expression template in the current context.
compileRegex :: Template -> Reforg Regex
compileRegex t = either (throwError . ReforgErrorRegex) pure =<< asks (mkRegex . T.unpack . renderTemplate t . compileContext)


-- | Helper function that checks if the given file is excluded from processing as per given regular expressions.
isExcluded :: [Regex] -> Path Abs File -> Bool
isExcluded rs f = any (`isMatch` (toFilePath $ filename f)) rs


-- | Helper function that checks if the given file is included in processing as per given regular expressions.
isIncluded :: [Regex] -> Path Abs File -> Bool
isIncluded rs = not . isExcluded rs

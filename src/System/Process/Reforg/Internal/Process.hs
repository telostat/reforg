{-# LANGUAGE DataKinds #-}
module System.Process.Reforg.Internal.Process where

import           Control.Monad.Except                      (throwError)
import           Control.Monad.Reader                      (MonadIO(liftIO), asks, when)
import           Data.Bifunctor                            (bimap)
import qualified Data.HashMap.Strict                       as HM
import           Data.Maybe                                (isNothing)
import qualified Data.Text                                 as T
import           System.Exit                               (ExitCode(..))
import           System.IO                                 (Handle)
import           System.Process                            (createPipe)
import           System.Process.Reforg.Internal.Class      (Env(..), Reforg, ReforgError(..))
import           System.Process.Reforg.Internal.Contextual (compileContext, updateExecEnv)
import           System.Process.Reforg.Internal.Templating (Template, renderTemplate)
import           System.Process.Reforg.Internal.Types      (Command(..), When(..))
import qualified System.Process.Typed                      as P


checkCondition :: When -> Reforg Bool
checkCondition (WhenSh     t) = renderWhenCommand t >>= runWhen "sh" "-c"
checkCondition (WhenBash   t) = renderWhenCommand t >>= runWhen "bash" "-c"
checkCondition (WhenPython t) = renderWhenCommand t >>= runWhen "python" "-c"


runWhen :: String -> String -> String -> Reforg Bool
runWhen c f a = (ExitSuccess == ) <$> P.runProcess (P.proc c [f, a])


renderWhenCommand :: Template -> Reforg String
renderWhenCommand t = asks (T.unpack . renderTemplate t . compileContext)


-- | Runs the file as per matched rule.
runCommands :: [Command] -> Maybe Handle -> Reforg ()
runCommands [] _ = pure ()
runCommands (c : cs) mi = do
  -- Guard: Check if we need a STDIN AND we have been provided one:
  when (execStdin c && isNothing mi) $ throwError (ReforgErrorProcess "Command needs STDIN but process does not provide one")
  -- Compile the command context:
  ctx <- asks (compileContext . updateExecEnv c)
  -- Compile executable:
  let exec = T.unpack $ renderTemplate (execExecutable c) ctx
  -- Compile arguments:
  let args = fmap (T.unpack . flip renderTemplate ctx) (execArguments c)
  -- Standard output:
  (soRead, soWrite) <- liftIO createPipe
  -- Get environment variables:
  envars <- asks envEnvars
  -- Create the process configuration:
  let pc = setEnv envars . setStdout soWrite . setStdin $ P.proc exec args
  -- Run the process:
  ec <- P.runProcess pc
  -- Check the exit code:
  case ec of
    ExitSuccess -> runCommands cs (if execStdout c then Just soRead else Nothing)
    ExitFailure n -> throwError $ ReforgErrorProcess $ "Process has exited with: " <> show n <> ". Process was: " <> show pc
  where
    setStdin = maybe id (P.setStdin . P.useHandleClose) mi
    setStdout h = if execStdout c then P.setStdout (P.useHandleClose h) else id
    setEnv e = P.setEnv (bimap T.unpack T.unpack <$> HM.toList e)

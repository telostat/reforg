module Main where

import           Colog                                (richMessageAction)
import           Control.Monad.IO.Class               (MonadIO(..))
import           Data.Version                         (showVersion)
import           Data.Yaml                            (decodeFileEither)
import qualified Options.Applicative                  as OA
import           Path                                 (Abs, Dir, File, Path, toFilePath)
import           Path.IO                              (doesDirExist, doesFileExist, resolveDir', resolveFile')
import           Paths_reforg                         (version)
import           System.Exit                          (ExitCode(..), die, exitWith)
import           System.IO                            (hPutStrLn, stderr)
import           System.Process.Reforg                (program)
import           System.Process.Reforg.Internal.Class (Env(Env), ReforgError(..), runReforg)
import           System.Process.Reforg.Internal.Types (Spec)


-- | Main program entry point.
main :: IO ()
main = exitWith =<< (cliProgram =<< OA.execParser cliProgramParserInfo)


-- | CLI program.
cliProgram :: CliArguments -> IO ExitCode
cliProgram (CliArguments (CommandProcess (s, n, ds))) = process s n ds


-- | Processes given CSV data with given specification.
process :: MonadIO m => FilePath -> Bool -> [FilePath] -> m ExitCode
process fpSpec dryrun dirs = do
  spSpec <- either (liftIO . die) pure =<< ensureFile fpSpec
  spDirs <- either (liftIO . die) pure . sequence =<< mapM ensureDir dirs
  spec <- either (liftIO . die . show) (\x -> pure (x :: Spec)) =<< liftIO (decodeFileEither $ toFilePath spSpec)
  result <- liftIO $ runReforg (Env spec dryrun richMessageAction) (program spDirs)
  case result of
    Left (ReforgErrorSpec    x) -> liftIO (hPutStrLn stderr $ "Specification Error: " <> x) >> pure (ExitFailure 1)
    Left (ReforgErrorIO      x) -> liftIO (hPutStrLn stderr $ "I/O Error: " <> x) >> pure (ExitFailure 2)
    Left (ReforgErrorProcess x) -> liftIO (hPutStrLn stderr $ "Subprocess Error: " <> x) >> pure (ExitFailure 3)
    Left (ReforgErrorUnknown x) -> liftIO (hPutStrLn stderr $ "Unknown Error: " <> x) >> pure (ExitFailure 42)
    Right _                     -> pure ExitSuccess


-- | Attempts to ensure that the given file path is transformed into an absolute
-- file path and it exists.
ensureFile :: MonadIO m => FilePath -> m (Either String (Path Abs File))
ensureFile fp = do
  sap <- resolveFile' fp
  exists <- doesFileExist sap
  pure $ if exists
    then Right sap
    else Left $ "File does not exist: " <> fp


-- | Attempts to ensure that the given directory path is transformed into an absolute
-- directory path and it exists.
ensureDir :: MonadIO m => FilePath -> m (Either String (Path Abs Dir))
ensureDir fp = do
  sap <- resolveDir' fp
  exists <- doesDirExist sap
  pure $ if exists
    then Right sap
    else Left $ "Directory does not exist: " <> fp


-- | CLI arguments parser.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  ( OA.command "process" (OA.info (CommandProcess <$> optsProcess) (OA.progDesc "Process files as per given specification file"))
  )


-- | @process@ command arguments parser.
optsProcess :: OA.Parser (FilePath, Bool, [FilePath])
optsProcess = (,,)
  <$> OA.strOption (OA.long "spec" <> OA.short 's' <> OA.metavar "SPEC" <> OA.help "Path to reforg specification file (YAML or JSON)")
  <*> OA.switch (OA.long "dry-run" <> OA.short 'n' <> OA.help "Dry run")
  <*> OA.some (OA.argument OA.str (OA.metavar "DIR [DIR...]" <> OA.help "Paths to directories containing files to process"))


-- | Registry of commands.
newtype Command = CommandProcess (FilePath, Bool, [FilePath])
  deriving Show


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command } deriving Show


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo CliArguments
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "Reforg" <> OA.header "reforg - Organize and Process Files in Bulk")


-- | Version option.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")

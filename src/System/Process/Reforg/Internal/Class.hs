-- | Definitions for monadic Reforg program definitions.
--
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module System.Process.Reforg.Internal.Class where

import           Colog                  (HasLog(..), LogAction, Message)
import           Control.Monad.Except   (ExceptT(..), MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT(..))
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Path                   (Abs, File, Path)


-- | Reforg program MTS definition.
newtype Reforg a = Reforg
    { unReforg :: ExceptT ReforgError (ReaderT (Env Reforg) IO) a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError ReforgError, MonadReader (Env Reforg))


-- | Runs a given 'Reforg' program in a given 'Reforg' environment ('Env').
runReforg :: Env Reforg -> Reforg a -> IO (Either ReforgError a)
runReforg env app = runReaderT (runExceptT (unReforg app)) env


-- | Encoding for possible errors we may encounter while executing Reforg
-- programs.
data ReforgError =
    ReforgErrorIO String
  | ReforgErrorNoMatch (Path Abs File)
  | ReforgErrorProcess String
  | ReforgErrorRegex String
  | ReforgErrorSpec String
  | ReforgErrorUnknown String
  deriving (Show)


-- | Provides Reforg program environment.
data Env m = Env
  { envEnvars    :: !(HM.HashMap T.Text T.Text)
  , envParams    :: !(HM.HashMap T.Text T.Text)
  , envRevars    :: !(HM.HashMap T.Text T.Text)
  , envFsvars    :: !(HM.HashMap T.Text T.Text)
  , envDryrun    :: !Bool
  , envLogAction :: !(LogAction m Message)  -- ^ Logger action implementation
  }


-- | 'HasLog' instance for 'Env'.
instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

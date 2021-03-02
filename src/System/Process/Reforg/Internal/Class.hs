{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module System.Process.Reforg.Internal.Class where

import Colog                                (HasLog(..), LogAction, Message)
import Control.Monad.Except                 (ExceptT(..), MonadError, runExceptT)
import Control.Monad.IO.Class               (MonadIO)
import Control.Monad.Reader                 (MonadReader, ReaderT(..), asks)
import System.Process.Reforg.Internal.Types (Spec)


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
    ReforgErrorSpec String
  | ReforgErrorIO String
  | ReforgErrorProcess String
  | ReforgErrorUnknown String
  deriving (Show)


-- | Provides Reforg program environment.
data Env m = Env
  { envSpec      :: !Spec
  , envDryrun    :: !Bool
  , envLogAction :: !(LogAction m Message)  -- ^ Logger action implementation
  }


instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { envLogAction = newLogAction }
    {-# INLINE setLogAction #-}


-- | Retrieves specification from the environment.
askSpec :: MonadReader (Env m) m => m Spec
askSpec = asks envSpec


-- | Retrieves dry-run indicator from the environment.
askDryrun :: MonadReader (Env m) m => m Bool
askDryrun = asks envDryrun

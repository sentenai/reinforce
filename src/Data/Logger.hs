{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Logger where

import Reinforce.Prelude
import Control.MonadMWCRandom
import Servant.Client
import Debug.Trace
import Control.MonadEnv.Internal
import qualified Data.Text as T

-- ========================================================================= --

data Event r o a = Event Integer r o a
  deriving Show

-- ========================================================================= --


class Monad m => Logger m where
  info   :: Text -> m ()
  info_  :: Text -> Text -> m ()
  debug  :: Text -> m ()
  debug_ :: Text -> Text -> m ()

instance (Logger m) => Logger (StateT s m) where
  info   a   = lift $ info   a
  info_  a b = lift $ info_  a b
  debug  a   = lift $ debug  a
  debug_ a b = lift $ debug_ a b

instance (Logger m) => Logger (MWCRandT m) where
  info   a   = lift $ info   a
  info_  a b = lift $ info_  a b
  debug  a   = lift $ debug  a
  debug_ a b = lift $ debug_ a b

instance (Logger m, Monoid w) => Logger (RWST r w s m) where
  info   a   = lift $ info   a
  info_  a b = lift $ info_  a b
  debug  a   = lift $ debug  a
  debug_ a b = lift $ debug_ a b

instance Logger ClientM where
  info   a   = traceM $ T.unpack a
  info_  a b = traceM $ T.unpack (a <> b)
  debug  a   = traceM $ T.unpack a
  debug_ a b = traceM $ T.unpack (a <> b)

instance Logger IO where
  info   a   = traceM $ T.unpack a
  info_  a b = traceM $ T.unpack (a <> b)
  debug  a   = traceM $ T.unpack a
  debug_ a b = traceM $ T.unpack (a <> b)

-- ========================================================================= --


newtype NoopLogger m x = NoopLogger { runNoopLogger :: m x }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance Monad m => Logger (NoopLogger m) where
  info   _   = return ()
  info_  _ _ = return ()
  debug  _   = return ()
  debug_ _ _ = return ()

instance MonadEnv m s a r => MonadEnv (NoopLogger m) s a r where
  reset = NoopLogger reset
  step a = NoopLogger $ step a
  -- runAction = NoopLogger . runAction
  -- reward = NoopLogger . reward

newtype DebugLogger m x = DebugLogger { runDebugLogger :: m x }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance Monad m => Logger (DebugLogger m) where
  info   a   = traceM $ T.unpack a
  info_  a b = traceM $ T.unpack (a <> b)
  debug  a   = traceM $ T.unpack a
  debug_ a b = traceM $ T.unpack (a <> b)

instance MonadEnv m s a r => MonadEnv (DebugLogger m) s a r where
  reset = DebugLogger reset
  step a = DebugLogger $ step a
  -- runAction = DebugLogger . runAction
  -- reward = DebugLogger . reward


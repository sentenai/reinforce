-------------------------------------------------------------------------------
-- |
-- Module    :  Data.Logger
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- In lieu of a history monad embedded in models maintain a logger monad for
-- easily debugging environments.
-------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Logger
  ( Event(..)
  , Logger(..)
  , NoopLogger(..)
  , DebugLogger(..)
  ) where

import Reinforce.Prelude
import Control.MonadMWCRandom
import Servant.Client
import Debug.Trace
import Control.MonadEnv
import qualified Data.Text as T

-- ========================================================================= --
-- | Our primary datatype for an event in a trace. Contains the episode number,
-- reward, state, and action taken (in that order).
-- TODO: change the ordering to @Event Integer s a r@
data Event r o a = Event Integer r o a
  deriving Show

-- ========================================================================= --

-- | A logging monad, this is seperate from a History monad in that this is
-- intended to be used for debugging and for toggling log information.
--
-- FIXME: In reality, this is halfway between commenting/uncommenting print
-- statements, and passing CPP flags. Something should be done about this
-- or a real logging Monad should be brought in.
class Monad m => Logger m where

  -- | log at the 'info' level.
  info   :: Text -> m ()

  -- | log at the 'info' level, appending information on the left.
  info_  :: Text -> Text -> m ()

  -- | log at the 'debug' level.
  debug  :: Text -> m ()

  -- | log at the 'debug' level, appending information on the left.
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

-- | A prebuilt type which doesn't actually log anything.
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


-- | A prebuilt type that does all levels of logging
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


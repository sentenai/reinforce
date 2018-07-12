-------------------------------------------------------------------------------
-- |
-- Module    :  Classifiers.RL.Control.MonadEnv
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- User-facing API for MonadEnv, typeclass used to implement an environment
-------------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.MonadEnv
  ( MonadEnv(..)
  , Obs(..)
  , Initial(..)
  , Reward
  ) where

-- import Control.Monad.Reader.Class
-- import Control.Monad.RWS.Class
-- import Control.Monad.IO.Class
-- import Control.Monad.Writer.Class
-- import Control.Monad.State.Class
-- import Control.Monad.Trans
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans
-- import Data.Hashable
-- import Data.Maybe (fromMaybe)
-- import Lens.Micro.Platform


-- * Environment Types

-- | A concrete reward signal.
type Reward = Double

-- | When starting an episode, we want to send an indication that the environment
-- is starting without conflating this type with future steps (in @Obs r o@)
data Initial o = Initial !o | EmptyEpisode

-- | An observation of the environment will either show that the environment is
-- done with the episode (yielding 'Done'), that the environment has already
-- 'Terminated', or will return the reward of the last action performed and the
-- next state
-- TODO: return @Terminal@ (or return ()) on failure
-- FIXME: do we always return o on done?
data Obs r o = Next !r !o | Done !r !(Maybe o) | Terminated
  deriving (Show, Eq)


-- * The Environment Monad

-- | The environment monad
-- TODO: Think about two typeclasses: ContinuousMonadEnv and EpisodicMonadEnv
class (Num r, Monad e) => MonadEnv e s a r | e -> s a r where
  -- | Any environment must be initialized with 'reset'. This can be used to
  -- reset the environment at any time. It's expected that resetting an
  -- environment begins a new episode (and can only be called once in a
  -- continuous environment).
  reset :: e (Initial s)

  -- | Step though an environment with an action, run the action in the
  -- environment, and return a reward and the new state of the environment.
  step :: a -> e (Obs r s)

  -- -- Perform an action given to the environment by an agent and run
  -- -- all effects in the environment
  -- runAction :: a -> e ()

  -- -- Calculate how much reward is given when running an action in the
  -- -- context of the environment
  -- reward :: a -> e r


-- ** lifted instances for MTL

instance MonadEnv e s a r => MonadEnv (ReaderT t e) s a r where
  reset :: ReaderT t e (Initial s)
  reset = lift reset

  step :: a -> ReaderT t e (Obs r s)
  step a = lift $ step a

instance MonadEnv e s a r => MonadEnv (StateT t e) s a r where
  reset :: StateT t e (Initial s)
  reset = lift reset

  step :: a -> StateT t e (Obs r s)
  step a = lift $ step a

instance (Monoid t, MonadEnv e s a r) => MonadEnv (WriterT t e) s a r where
  reset :: WriterT t e (Initial s)
  reset = lift reset

  step :: a -> WriterT t e (Obs r s)
  step a = lift $ step a

instance (Monoid writer, MonadEnv e s a r) => MonadEnv (RWST reader writer state e) s a r where
  reset :: RWST reader writer state e (Initial s)
  reset = lift reset

  step :: a -> RWST reader writer state e (Obs r s)
  step a = lift $ step a


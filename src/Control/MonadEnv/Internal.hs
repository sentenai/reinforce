-------------------------------------------------------------------------------
-- |
-- Module    :  Classifiers.RL.Control.MonadEnv.Internal
-- Copyright :  (c) Sentenai 2017
-- License   :  Proprietary
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Used to implement an environment
-------------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.MonadEnv.Internal where

import Reinforce.Prelude

-- * Environment Types

-- | The reward signal.
type Reward = Double

-- | An observation of the environment will either show that the environment is
-- done with the episode (yielding 'Done') or will return the reward of
-- the last action performed and the next state
data Obs r o = Done | Next !r !o
  deriving (Show, Eq)


instance (Num r, Monoid o) => Monoid (Obs r o) where
  mempty = Done

  Done `mappend` _ = Done
  _ `mappend` Done = Done
  Next r0 a `mappend` Next r1 b = Next (r0+r1) (a `mappend` b)


instance Functor (Obs r) where
  fmap _ Done = Done
  fmap fn (Next r o) = Next r (fn o)


instance Num r => Applicative (Obs r) where
  pure a = Next 0 a

  Done <*> _ = Done
  _ <*> Done = Done
  (Next _ fn) <*> (Next r a) = Next r (fn a)


instance Num r => Alternative (Obs r) where
  empty = Done

  Done <|> r@(Next _ _) = r
  l <|> _ = l


-- * The Environment Monad

-- | Our environment monad
class (Num r, Monad e, Enum a) => MonadEnv e s a r | e -> s a r where
  -- | A process (in this case an episode of json indexing) gets started
  -- by calling reset, which returns an initial observation.
  reset :: e (Obs r s)

  -- | Step though an environment using an action.
  step :: a -> r -> e (Obs r s)

  -- | Perform an action given to the environment by an agent and run
  -- all effects in the environment
  runAction :: a -> e ()

  -- | Calculate how much reward is given when running an action in the
  -- context of the environment
  reward :: a -> e r


-- * Basic Instances

instance MonadEnv e s a r => MonadEnv (ReaderT t e) s a r where
  reset :: ReaderT t e (Obs r s)
  reset = lift reset

  step :: a -> r -> ReaderT t e (Obs r s)
  step a = lift . step a

  runAction :: a -> ReaderT t e ()
  runAction = lift . runAction

  reward :: a -> ReaderT t e r
  reward = lift . reward


instance MonadEnv e s a r => MonadEnv (StateT t e) s a r where
  reset :: StateT t e (Obs r s)
  reset = lift reset

  step :: a -> r -> StateT t e (Obs r s)
  step a = lift . step a

  runAction :: a -> StateT t e ()
  runAction = lift . runAction

  reward :: a -> StateT t e r
  reward = lift . reward


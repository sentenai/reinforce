--------------------------------------------------------------------------------
-- |
-- Module    :  Environment.CartPoleV0
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Environment description:
-- > A pole is attached by an un-actuated joint to a cart, which moves along a
-- > frictionless track. The system is controlled by applying a force of +1 or -1
-- > to the cart. The pendulum starts upright, and the goal is to prevent it from
-- > falling over. A reward of +1 is provided for every timestep that the pole
-- > remains upright. The episode ends when the pole is more than 15 degrees from
-- > vertical, or the cart moves more than 2.4 units from the center.
--
-- https://gym.openai.com/envs/CartPole-v0
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Environments.Gym.ClassicControl.CartPoleV0
  ( Action(..)
  , I.Runner
  , StateCP(..)
  , Environment
  , EnvironmentT
  , Environments.Gym.ClassicControl.CartPoleV0.runEnvironment
  , Environments.Gym.ClassicControl.CartPoleV0.runEnvironmentT
  , Environments.Gym.ClassicControl.CartPoleV0.runDefaultEnvironment
  , Environments.Gym.ClassicControl.CartPoleV0.runDefaultEnvironmentT
  ) where


import Reinforce.Prelude
import Data.CartPole
import Control.MonadEnv (MonadEnv(..), Reward)
import Environments.Gym.Internal (GymEnvironmentT)
import qualified Environments.Gym.Internal as I
import OpenAI.Gym (GymEnv(CartPoleV0))
import Servant.Client (BaseUrl)
import Network.HTTP.Client (Manager)


-- ========================================================================= --
-- | Alias to 'Environments.Gym.Internal.GymEnvironmentT' with CartPoleV0 type dependencies
type EnvironmentT t = GymEnvironmentT StateCP Action t

-- | Alias to 'EnvironmentT' in IO
type Environment = EnvironmentT IO

-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT StateCP Action t x
runEnvironmentT = I.runEnvironmentT CartPoleV0

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT StateCP Action IO x
runEnvironment = I.runEnvironmentT CartPoleV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT StateCP Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT CartPoleV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT StateCP Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT CartPoleV0


instance (MonadIO t, MonadThrow t) => MonadEnv (EnvironmentT t) StateCP Action Reward where
  reset = I._reset
  step = I._step


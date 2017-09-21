--------------------------------------------------------------------------------
-- |
-- Module    :  Environment.Gym.ClassicControl.CartPoleV1
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
--
-- Environment description:
-- > A pole is attached by an un-actuated joint to a cart, which moves along a
-- > frictionless track. The system is controlled by applying a force of +1 or -1
-- > to the cart. The pendulum starts upright, and the goal is to prevent it from
-- > falling over. A reward of +1 is provided for every timestep that the pole
-- > remains upright. The episode ends when the pole is more than 15 degrees from
-- > vertical, or the cart moves more than 2.4 units from the center.
-- >
-- > CartPole-v1 defines "solving" as getting average reward of 475.0 over 100
-- > consecutive trials.
-- >
-- > This environment corresponds to the version of the cart-pole problem
-- > described by Barto, Sutton, and Anderson [Barto83].
--
-- https://gym.openai.com/envs/CartPole-v1
--------------------------------------------------------------------------------
module Environments.Gym.ClassicControl.CartPoleV1
  ( Env.Action(..)
  , I.Runner
  , Env.StateCP(..)
  , Env.Environment
  , Env.EnvironmentT
  , runEnvironment
  , runEnvironmentT
  , runDefaultEnvironment
  , runDefaultEnvironmentT
  ) where

import Reinforce.Prelude hiding (State)
import Data.CartPole
import OpenAI.Gym (GymEnv(CartPoleV1))
import Servant.Client (BaseUrl)
import Network.HTTP.Client (Manager)
import Environments.Gym.ClassicControl.CartPoleV0 as Env hiding
  ( runEnvironment
  , runEnvironmentT
  , runDefaultEnvironmentT
  , runDefaultEnvironment
  )
import qualified Environments.Gym.Internal as I


-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT StateCP Action t x
runEnvironmentT = I.runEnvironmentT CartPoleV1

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT StateCP Action IO x
runEnvironment = I.runEnvironmentT CartPoleV1

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT StateCP Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT CartPoleV1

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT StateCP Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT CartPoleV1


--------------------------------------------------------------------------------
-- Module : Environment.Gym.ClassicControl.CartPoleV1
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
  ( module Env
  , runEnvironment
  , runDefaultEnvironment
  ) where

import OpenAI.Gym (GymEnv(CartPoleV1))
import Environments.Gym.ClassicControl.CartPoleV0 as Env hiding (runEnvironment, runDefaultEnvironment)
import qualified Environments.Gym.Internal as I

runEnvironment = I.runEnvironment CartPoleV1
runDefaultEnvironment = I.runDefaultEnvironment CartPoleV1


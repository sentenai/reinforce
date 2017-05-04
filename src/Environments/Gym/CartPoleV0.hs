--------------------------------------------------------------------------------
-- Module : Environment.CartPoleV0
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
module Environments.Gym.CartPoleV0 where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import Data.CartPole
import Environments.Gym.Internal hiding (runEnvironment, getEnvironment)
import qualified Environments.Gym.Internal as I
import OpenAI.Gym (GymEnv(CartPoleV0))


type Environment = GymEnvironment StateCP Action

runEnvironment = I.runEnvironment CartPoleV0
runDefaultEnvironment = I.runDefaultEnvironment CartPoleV0

instance MonadEnv Environment StateCP Action Reward where
  reset = I._reset
  step = I._step


--------------------------------------------------------------------------------
-- Module : Environments.Gym.ClassicControl.MountainCarContinuousV0
--
-- Environment description:
-- > A car is on a one-dimensional track, positioned between two "mountains".
-- > The goal is to drive up the mountain on the right; however, the car's
-- > engine is not strong enough to scale the mountain in a single pass.
-- > Therefore, the only way to succeed is to drive back and forth to build up
-- > momentum. Here, the reward is greater if you spend less energy to reach the
-- > goal.
-- >
-- > MountainCarContinuous-v0 defines "solving" as getting average reward of
-- > 90.0 over 100 consecutive trials.
-- >
-- > This problem was first described by Andrew Moore in his PhD thesis [Moore90].
-- >
-- > Here, this is the continuous version.
--
-- https://gym.openai.com/envs/MountainCarContinuous-v0
--------------------------------------------------------------------------------
module Environments.Gym.ClassicControl.MountainCarContinuousV0
  ( module Env
  , runEnvironment
  , runDefaultEnvironment
  ) where

import OpenAI.Gym (GymEnv(MountainCarContinuousV0))
import Environments.Gym.ClassicControl.MountainCarV0 as Env hiding (runEnvironment, runDefaultEnvironment)
import qualified Environments.Gym.Internal as I

runEnvironment = I.runEnvironment MountainCarContinuousV0
runDefaultEnvironment = I.runDefaultEnvironment MountainCarContinuousV0


--------------------------------------------------------------------------------
-- |
-- Module    :  Environments.Gym.ClassicControl.MountainCarContinuousV0
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
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
  ( Env.Action(..)
  , Env.Runner
  , Env.State(..)
  , Env.Environment
  , Env.EnvironmentT
  , runEnvironment
  , runEnvironmentT
  , runDefaultEnvironment
  , runDefaultEnvironmentT
  ) where

import Reinforce.Prelude hiding (State)
import OpenAI.Gym (GymEnv(MountainCarContinuousV0))
import Environments.Gym.ClassicControl.MountainCarV0 as Env hiding
  ( runEnvironment
  , runDefaultEnvironment
  , runEnvironmentT
  , runDefaultEnvironmentT
  )
import qualified Environments.Gym.Internal as I

-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT State Action t x
runEnvironmentT = I.runEnvironmentT MountainCarContinuousV0

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT State Action IO x
runEnvironment = I.runEnvironmentT MountainCarContinuousV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT State Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT MountainCarContinuousV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT State Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT MountainCarContinuousV0


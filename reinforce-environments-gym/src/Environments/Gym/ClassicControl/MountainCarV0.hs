--------------------------------------------------------------------------------
-- |
-- Module    :  Environments.Gym.ClassicControl.MountainCarV0
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
-- > momentum.
-- >
-- > MountainCar-v0 defines "solving" as getting average reward of -110.0 over
-- > 100 consecutive trials.
-- >
-- > This problem was first described by Andrew Moore in his PhD thesis
-- > [Moore90].
--
-- https://gym.openai.com/envs/MountainCar-v0
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Environments.Gym.ClassicControl.MountainCarV0
  ( Action(..)
  , I.Runner
  , State(..)
  , Environment
  , EnvironmentT
  , Environments.Gym.ClassicControl.MountainCarV0.runEnvironment
  , Environments.Gym.ClassicControl.MountainCarV0.runEnvironmentT
  , Environments.Gym.ClassicControl.MountainCarV0.runDefaultEnvironment
  , Environments.Gym.ClassicControl.MountainCarV0.runDefaultEnvironmentT
  ) where


import Reinforce.Prelude hiding (State)
import Control.MonadEnv
import Environments.Gym.Internal hiding (runEnvironment)
import qualified Environments.Gym.Internal as I

import Data.Aeson.Types
import OpenAI.Gym (GymEnv(MountainCarV0))


-- | State of a car stuck between two hills
data State = State
  { position :: Float
  , velocity :: Float
  } deriving (Show, Eq, Generic, Ord, Hashable)


instance FromJSON State where
  parseJSON :: Value -> Parser State
  parseJSON arr@(Array _)= do
    (a, b) <- parseJSON arr :: Parser (Float, Float)
    return $ State a b
  parseJSON invalid = typeMismatch "Environment State" invalid


-- | Actions a car can perform to get out of it's predicament
data Action = MoveLeft | DoNothing | MoveRight
  deriving (Enum, Bounded, Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . subtract 1 . fromEnum

-- ========================================================================= --
-- | Alias to 'Environments.Gym.Internal.GymEnvironmentT' with MountainCarV0 type dependencies
type EnvironmentT t = GymEnvironmentT State Action t

-- | Alias to 'EnvironmentT' in IO
type Environment = EnvironmentT IO

-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT State Action t x
runEnvironmentT = I.runEnvironmentT MountainCarV0

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT State Action IO x
runEnvironment = I.runEnvironmentT MountainCarV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT State Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT MountainCarV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT State Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT MountainCarV0

instance (MonadThrow t, MonadIO t) => MonadEnv (EnvironmentT t) State Action Reward where
  reset = I._reset
  step = I._step


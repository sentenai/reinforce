--------------------------------------------------------------------------------
-- |
-- Module    :  Environment.Gym.ClassicControl.AcrobotV1
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The inverted pendulum swingup problem is a classic problem in the control
-- literature. In this version of the problem, the pendulum starts in a random
-- position, and the goal is to swing it up so it stays upright.
--
-- https://gym.openai.com/envs/Acrobot-v1
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Environments.Gym.ClassicControl.AcrobotV1
  ( Action(..)
  , I.Runner
  , State(..)
  , Environment
  , EnvironmentT
  , Environments.Gym.ClassicControl.AcrobotV1.runEnvironment
  , Environments.Gym.ClassicControl.AcrobotV1.runEnvironmentT
  , Environments.Gym.ClassicControl.AcrobotV1.runDefaultEnvironment
  , Environments.Gym.ClassicControl.AcrobotV1.runDefaultEnvironmentT
  ) where


import Reinforce.Prelude hiding (State)
import Control.MonadEnv (MonadEnv(..), Reward)
import Environments.Gym.Internal (GymEnvironmentT)
import qualified Environments.Gym.Internal as I

import Data.Aeson.Types
import OpenAI.Gym (GymEnv(AcrobotV1))


-- | The Acrobot's State
-- FIXME: keep semantics or move to a tuple?
data State = State
  { cosVel0     :: Float
  , sinVel0     :: Float
  , cosVel1     :: Float
  , sinVel1     :: Float
  , joint0Angle :: Float
  , joint1Angle :: Float
  } deriving (Show, Eq, Generic, Ord, Hashable)


instance FromJSON State where
  parseJSON :: Value -> Parser State
  parseJSON arr@(Array _)= do
    (a, b, c, d, e, f) <- parseJSON arr :: Parser (Float, Float, Float, Float, Float, Float)
    return $ State a b c d e f
  parseJSON invalid = typeMismatch "Environment State" invalid

-- | Acrobot Actions
data Action = RightTorque | NoTorque | LeftTorque
  deriving (Enum, Bounded, Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . subtract 1 . fromEnum

-- ========================================================================= --
-- | Alias to 'Environments.Gym.Internal.GymEnvironmentT' with AcrobotV1 type dependencies
type EnvironmentT t = GymEnvironmentT State Action t

-- | Alias to 'EnvironmentT' in IO
type Environment = EnvironmentT IO

-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT State Action t x
runEnvironmentT = I.runEnvironmentT AcrobotV1

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT State Action IO x
runEnvironment = I.runEnvironmentT AcrobotV1

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT State Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT AcrobotV1

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT State Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT AcrobotV1


instance (MonadIO t, MonadThrow t) => MonadEnv (EnvironmentT t) State Action Reward where
  reset = I._reset
  step = I._step


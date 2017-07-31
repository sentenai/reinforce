--------------------------------------------------------------------------------
-- Module : Environment.Gym.ClassicControl.AcrobotV1
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
module Environments.Gym.ClassicControl.AcrobotV1 where

import Reinforce.Prelude hiding (State)
import Control.MonadEnv (MonadEnv(..), Reward)
import Environments.Gym.Internal (GymEnvironmentT)
import qualified Environments.Gym.Internal as I

import Data.Aeson.Types
import OpenAI.Gym (GymEnv(AcrobotV1))


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


data Action = RightTorque | NoTorque | LeftTorque
  deriving (Enum, Bounded, Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . subtract 1 . fromEnum

-- ========================================================================= --

type EnvironmentT t = GymEnvironmentT State Action t
type Environment = EnvironmentT IO

runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT State Action t x
runEnvironmentT = I.runEnvironmentT AcrobotV1

runEnvironment :: Manager -> BaseUrl -> I.RunnerT State Action IO x
runEnvironment = I.runEnvironmentT AcrobotV1

runDefaultEnvironmentT :: MonadIO t => I.RunnerT State Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT AcrobotV1

runDefaultEnvironment :: I.RunnerT State Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT AcrobotV1

instance (MonadIO t, MonadThrow t) => MonadEnv (EnvironmentT t) State Action Reward where
  reset = I._reset
  step = I._step


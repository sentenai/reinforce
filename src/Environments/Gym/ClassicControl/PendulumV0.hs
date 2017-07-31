--------------------------------------------------------------------------------
-- Module : Environment.Gym.ClassicControl.PendulumV0
--
-- The inverted pendulum swingup problem is a classic problem in the control
-- literature. In this version of the problem, the pendulum starts in a random
-- position, and the goal is to swing it up so it stays upright.
--
-- https://gym.openai.com/envs/Pendulum-v0
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Environments.Gym.ClassicControl.PendulumV0 where

import Reinforce.Prelude hiding (State)
import Control.MonadEnv (MonadEnv(..), Reward)
import Environments.Gym.Internal (GymEnvironmentT)
import qualified Environments.Gym.Internal as I

import Data.Aeson.Types
import OpenAI.Gym (GymEnv(PendulumV0))


-- FIXME: give these semantics or move to a tuple?
data State = State
  { cosTheta  :: Float
  , sinTheta  :: Float
  , thetaDot  :: Float
  } deriving (Show, Eq, Generic, Ord, Hashable)


instance FromJSON State where
  parseJSON :: Value -> Parser State
  parseJSON arr@(Array _)= do
    (a, b, c) <- parseJSON arr :: Parser (Float, Float, Float)
    return $ State a b c
  parseJSON invalid = typeMismatch "Environment State" invalid


newtype Action = Action { getAction :: Float }
  deriving (Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . getAction

-- ========================================================================= --

type EnvironmentT t = GymEnvironmentT State Action t
type Environment = EnvironmentT IO

runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT State Action t x
runEnvironmentT = I.runEnvironmentT PendulumV0

runEnvironment :: Manager -> BaseUrl -> I.RunnerT State Action IO x
runEnvironment = I.runEnvironmentT PendulumV0

runDefaultEnvironmentT :: MonadIO t => I.RunnerT State Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT PendulumV0

runDefaultEnvironment :: I.RunnerT State Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT PendulumV0

instance (MonadIO t, MonadThrow t) => MonadEnv (EnvironmentT t) State Action Reward where
  reset = I._reset
  step = I._step


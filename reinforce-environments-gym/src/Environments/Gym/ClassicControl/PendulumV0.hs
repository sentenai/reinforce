--------------------------------------------------------------------------------
-- |
-- Module    :  Environment.Gym.ClassicControl.PendulumV0
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
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
module Environments.Gym.ClassicControl.PendulumV0
  ( Action(..)
  , I.Runner
  , State(..)
  , Environment
  , EnvironmentT
  , Environments.Gym.ClassicControl.PendulumV0.runEnvironment
  , Environments.Gym.ClassicControl.PendulumV0.runEnvironmentT
  , Environments.Gym.ClassicControl.PendulumV0.runDefaultEnvironment
  , Environments.Gym.ClassicControl.PendulumV0.runDefaultEnvironmentT
  ) where

-- import Reinforce.Prelude hiding (State)
import Control.Monad.IO.Class
import Control.Exception.Safe
import Data.Hashable
import GHC.Generics
import Control.MonadEnv (MonadEnv(..), Reward)
import Environments.Gym.Internal (GymEnvironmentT)
import qualified Environments.Gym.Internal as I

import Data.Aeson.Types
import OpenAI.Gym (GymEnv(PendulumV0))
import Servant.Client as X (BaseUrl)
import Network.HTTP.Client as X (Manager)


-- | State of a PendulumV0 environment
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

-- | Force to exert on the pendulum
newtype Action = Action { getAction :: Float }
  deriving (Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . getAction

-- ========================================================================= --
-- | Alias to 'Environments.Gym.Internal.GymEnvironmentT' with PendulumV0 type dependencies
type EnvironmentT t = GymEnvironmentT State Action t

-- | Alias to 'EnvironmentT' in IO
type Environment = EnvironmentT IO

-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT State Action t x
runEnvironmentT = I.runEnvironmentT PendulumV0

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT State Action IO x
runEnvironment = I.runEnvironmentT PendulumV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT State Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT PendulumV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT State Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT PendulumV0


instance (MonadIO t, MonadThrow t) => MonadEnv (EnvironmentT t) State Action Reward where
  reset = I._reset
  step = I._step


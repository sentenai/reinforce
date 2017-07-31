--------------------------------------------------------------------------------
-- Module : Environment.Gym.ToyText.FrozenLakeV0
--
-- The agent controls the movement of a character in a grid world. Some tiles of
-- the grid are walkable, and others lead to the agent falling into the water.
-- Additionally, the movement direction of the agent is uncertain and only
-- partially depends on the chosen direction. The agent is rewarded for
-- finding a walkable path to a goal tile.
--
-- https://gym.openai.com/envs/FrozenLake-v0
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module Environments.Gym.ToyText.FrozenLakeV0
  ( I.Runner
  , StateFL(..)
  , toVector
  , mkStateFL
  , Environment
  , EnvironmentT
  , Environments.Gym.ToyText.FrozenLakeV0.runEnvironment
  , Environments.Gym.ToyText.FrozenLakeV0.runEnvironmentT
  , Environments.Gym.ToyText.FrozenLakeV0.runDefaultEnvironment
  , Environments.Gym.ToyText.FrozenLakeV0.runDefaultEnvironmentT
  , Action(..)
  ) where

import Reinforce.Prelude
import Control.MonadEnv
import Environments.Gym.Internal hiding (runEnvironment, getEnvironment)
import qualified Environments.Gym.Internal as I

import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.Types
import OpenAI.Gym (GymEnv(FrozenLakeV0))


newtype StateFL = Position { unPosition :: Int }
  deriving (Show, Eq, Generic, Ord, Hashable)


toVector :: StateFL -> Vector Int
toVector (Position p) = V.generate 16 (\i -> fromEnum (i == p))


mkStateFL :: MonadThrow m => Int -> m StateFL
mkStateFL i
  | i < 16 && i >= 0 = pure $ Position i
  |        otherwise = throwString $ "no state exists for " ++ show i

instance FromJSON StateFL where
  parseJSON :: Value -> Parser StateFL
  parseJSON n@(Number _) = parseJSON n >>= pure . Position
  parseJSON invalid      = typeMismatch "StateFL" invalid


data Action = Left | Down | Right | Up
  deriving (Enum, Bounded, Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . fromEnum

-- ========================================================================= --
type EnvironmentT t = GymEnvironmentT StateFL Action t
type Environment = EnvironmentT IO

runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT StateFL Action t x
runEnvironmentT = I.runEnvironmentT FrozenLakeV0

runEnvironment :: Manager -> BaseUrl -> I.RunnerT StateFL Action IO x
runEnvironment = I.runEnvironmentT FrozenLakeV0

runDefaultEnvironmentT :: MonadIO t => I.RunnerT StateFL Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT FrozenLakeV0

runDefaultEnvironment :: I.RunnerT StateFL Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT FrozenLakeV0

instance (MonadThrow t, MonadIO t) => MonadEnv (EnvironmentT t) StateFL Action Reward where
  reset = I._reset
  step = I._step


--------------------------------------------------------------------------------
-- |
-- Module    :  Environment.Gym.ToyText.FrozenLakeV0
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
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

import Control.Monad.IO.Class
import Control.Exception.Safe
import Data.Hashable
import Data.Vector (Vector)
import GHC.Generics

import Control.MonadEnv
import Environments.Gym.Internal hiding (runEnvironment)
import qualified Environments.Gym.Internal as I

import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.Types
import OpenAI.Gym (GymEnv(FrozenLakeV0))
import Servant.Client as X (BaseUrl)
import Network.HTTP.Client as X (Manager)


-- | The current position of the agent on the frozen lake
newtype StateFL = Position { unPosition :: Int }
  deriving (Show, Eq, Generic, Ord, Hashable)

-- | Convert 'StateFL' to a computable type
toVector :: StateFL -> Vector Int
toVector (Position p) = V.generate 16 (\i -> fromEnum (i == p))

-- | Build a FrozenLakeV0 state, throwing if the position is out of bounds.
mkStateFL :: MonadThrow m => Int -> m StateFL
mkStateFL i
  | i < 16 && i >= 0 = pure $ Position i
  |        otherwise = throwString $ "no state exists for " ++ show i

instance FromJSON StateFL where
  parseJSON :: Value -> Parser StateFL
  parseJSON n@(Number _) = parseJSON n >>= pure . Position
  parseJSON invalid      = typeMismatch "StateFL" invalid

-- | Actions that can be performed in FrozenLakeV0
data Action = Left | Down | Right | Up
  deriving (Enum, Bounded, Ord, Show, Eq, Generic, Hashable)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . fromEnum

-- ========================================================================= --
-- | Alias to 'Environments.Gym.Internal.GymEnvironmentT' with FrozenLakeV0 type dependencies
type EnvironmentT t = GymEnvironmentT StateFL Action t

-- | Alias to 'EnvironmentT' in IO
type Environment = EnvironmentT IO

-- | Alias to 'Environments.Gym.Internal.runEnvironmentT'
runEnvironmentT :: MonadIO t => Manager -> BaseUrl -> I.RunnerT StateFL Action t x
runEnvironmentT = I.runEnvironmentT FrozenLakeV0

-- | Alias to 'Environments.Gym.Internal.runEnvironment' in IO
runEnvironment :: Manager -> BaseUrl -> I.RunnerT StateFL Action IO x
runEnvironment = I.runEnvironmentT FrozenLakeV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironmentT'
runDefaultEnvironmentT :: MonadIO t => I.RunnerT StateFL Action t x
runDefaultEnvironmentT = I.runDefaultEnvironmentT FrozenLakeV0

-- | Alias to 'Environments.Gym.Internal.runDefaultEnvironment' in IO
runDefaultEnvironment :: I.RunnerT StateFL Action IO x
runDefaultEnvironment = I.runDefaultEnvironmentT FrozenLakeV0

instance (MonadThrow t, MonadIO t) => MonadEnv (EnvironmentT t) StateFL Action Reward where
  reset = I._reset
  step = I._step


--------------------------------------------------------------------------------
-- Module : Environment.Gym.FrozenLakeV0
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
module Environments.Gym.FrozenLakeV0 where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import Environments.Gym.Internal hiding (runEnvironment, getEnvironment)
import qualified Environments.Gym.Internal as I

import Data.Aeson
import Data.Aeson.Types
import Data.DList (DList)
import OpenAI.Gym (GymEnv(FrozenLakeV0))


newtype StateFL = Position { unPosition :: Int }
  deriving (Show, Eq, Generic, Ord, Hashable)


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

type Environment = GymEnvironment StateFL Action

runEnvironment = I.runEnvironment FrozenLakeV0
runDefaultEnvironment = I.runDefaultEnvironment FrozenLakeV0

instance MonadEnv Environment StateFL Action Reward where
  reset = I._reset
  step = I._step


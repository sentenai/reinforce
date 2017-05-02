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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Environments.Gym.FrozenLakeV0 where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import Control.MonadMWCRandom
import Environments.Gym.Internal hiding (runEnvironment, getEnvironment)
import qualified Environments.Gym.Internal as I

import Data.DList
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T (pack)
import qualified OpenAI.Gym as OpenAI
import Servant.Client
import Data.Logger (Logger)
import qualified Data.Logger as Logger
import Network.HTTP.Client
import OpenAI.Gym
  ( GymEnv(..)
  , InstID(..)
  , Observation(..)
  )

newtype StateFL = Position { unPosition :: Int }
  deriving (Show, Eq, Generic, Ord)

mkStateFL :: MonadThrow m => Int -> m StateFL
mkStateFL i
  | i < 16 && i >= 0 = pure $ Position i
  |        otherwise = throwString $ "no state exists for " ++ show i

instance Hashable StateFL

instance FromJSON StateFL where
  parseJSON :: Value -> Parser StateFL
  parseJSON n@(Number _) = parseJSON n >>= pure . Position
  parseJSON invalid      = typeMismatch "StateFL" invalid

data Action = Left | Down | Right | Up
  deriving (Enum, Bounded, Ord, Show, Eq, Generic)

instance Hashable Action

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . fromEnum

type Event = Logger.Event Reward StateFL Action

newtype Environment a = Environment
  { getEnvironment :: RWST GymConfigs (DList Event) (LastState StateFL) ClientM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader GymConfigs
    , MonadWriter (DList Event)
    , MonadState (LastState StateFL)
    , MonadRWS GymConfigs (DList Event) (LastState StateFL)
    , Logger
    )


runEnvironment :: Manager -> BaseUrl -> Bool -> Environment a -> IO (Either ServantError (DList Event))
runEnvironment a b c d = I.runEnvironment FrozenLakeV0 a b c d

runDefaultEnvironment :: Bool -> Environment a -> IO (Either ServantError (DList Event))
runDefaultEnvironment = I.runDefaultEnvironment FrozenLakeV0

instance I.GymEnvironment Environment StateFL Action Reward where
  inEnvironment  = Environment . lift
  getEnvironment = getEnvironment

instance MonadMWCRandom Environment where
  getGen = Environment $ liftIO getGen



-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance MonadEnv Environment StateFL Action Reward where
  reset :: Environment (Initial StateFL)
  reset = I._reset

  step :: Action -> Environment (Obs Reward StateFL)
  step = I._step


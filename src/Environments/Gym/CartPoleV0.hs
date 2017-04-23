--------------------------------------------------------------------------------
-- Module : Environment.CartPoleV0
--
-- Environment description:
-- > A pole is attached by an un-actuated joint to a cart, which moves along a
-- > frictionless track. The system is controlled by applying a force of +1 or -1
-- > to the cart. The pendulum starts upright, and the goal is to prevent it from
-- > falling over. A reward of +1 is provided for every timestep that the pole
-- > remains upright. The episode ends when the pole is more than 15 degrees from
-- > vertical, or the cart moves more than 2.4 units from the center.
--
-- https://gym.openai.com/envs/CartPole-v0
--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Environments.Gym.CartPoleV0 where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import Data.CartPole
import Environments.Gym.Internal hiding (runEnvironment, getEnvironment)
import qualified Environments.Gym.Internal as I

import Data.DList
import Data.Aeson
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


newtype Environment a = Environment
  { getEnvironment :: RWST GymConfigs (DList Event) (LastState StateCP) ClientM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader GymConfigs
    , MonadWriter (DList Event)
    , MonadState (LastState StateCP)
    , MonadRWS GymConfigs (DList Event) (LastState StateCP)
    , Logger
    )

-------------------------------------------------------------------------------
-- Helper functions

runEnvironment :: Manager -> BaseUrl -> Bool -> Environment a -> IO (Either ServantError (DList Event))
runEnvironment = I.runEnvironment CartPoleV0

instance GymEnvironment Environment StateCP Action Reward where
  inEnvironment = Environment . lift
  getEnvironment = getEnvironment

toState :: MonadThrow m => Value -> m StateCP
toState o =
  case (fromJSON o :: Result StateCP) of
    Error str -> throw $ UnexpectedServerResponse str
    Success o -> return o


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance MonadEnv Environment StateCP Action Reward where
  reset :: Environment (Obs Reward StateCP)
  reset = I._reset toState

  step :: Action -> Environment (Obs Reward StateCP)
  step = I._step toState


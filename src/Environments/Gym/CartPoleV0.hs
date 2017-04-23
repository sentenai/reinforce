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

toState :: MonadThrow m => Reward -> Value -> m (Obs Reward StateCP)
toState r o =
  case (fromJSON o :: Result StateCP) of
    Error str -> throw $ UnexpectedServerResponse str
    Success o -> return $ Next r o


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance MonadEnv Environment StateCP Action Reward where
  reset :: Environment (Initial StateCP)
  reset = do
    i <- getInstID
    Observation o <- I.inEnvironment . OpenAI.envReset $ i
    Next _ s <- toState 0 o
    get >>= \case
      Uninitialized ep -> put $ LastState (ep+1) s
      LastState   ep _ -> put $ LastState (ep+1) s
    return $ Initial s

  step :: Action -> Environment (Obs Reward StateCP)
  step a = do
    get >>= \case
      Uninitialized _ -> throwM EnvironmentRequiresReset
      LastState _ _   -> return ()

    GymConfigs i mon <- Environment ask
    out <- I.inEnvironment . OpenAI.envStep i $ renderStep mon
    if OpenAI.done out
    then return $ Done (OpenAI.reward out)
    else do
      n@(Next r s) <- toState (OpenAI.reward out) (OpenAI.observation out)
      LastState ep prior <- get
      put $ LastState ep s
      tell . pure  $ Logger.Event ep r prior a
      return n
    where
      renderStep :: Bool -> OpenAI.Step
      renderStep = OpenAI.Step (toJSON a)


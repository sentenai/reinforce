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

newtype Environment a = Environment { getEnvironment :: RWST GymConfigs (DList Event) LastState ClientM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader GymConfigs
    , MonadWriter (DList Event)
    , MonadState LastState
    , MonadRWS GymConfigs (DList Event) LastState
    , Logger
    )

data GymConfigs
  = GymConfigs InstID Bool    -- ^ the instance id, as well as a flag of if we want to render the state

data LastState
  = LastState Integer StateCP -- ^ the episode number and last state
  | Uninitialized Integer     -- ^ a flag that the state is no longer initialized, and the current episode
  deriving (Eq, Show)

data GymException
  = UnexpectedServerResponse [Char]
  | TypeError [Char]
  | EnvironmentRequiresReset
  deriving (Show)

instance Exception GymException where

-------------------------------------------------------------------------------
-- Helper functions

runEnvironment :: Manager -> BaseUrl -> Bool -> Environment a -> IO (Either ServantError (DList Event))
runEnvironment m u mon env = runClientM action (ClientEnv m u)
  where
  action :: ClientM (DList Event)
  action = do
    i <- OpenAI.envCreate CartPoleV0
    (_, w) <- execRWST (getEnvironment renderableEnv) (GymConfigs i mon) (Uninitialized 0)
    OpenAI.envClose i
    return w

  renderableEnv :: Environment ()
  renderableEnv =
    if mon
    then withMonitor env
    else env >> return ()


getInstID :: Environment InstID
getInstID = Environment $ ask >>= \(GymConfigs i _) -> return i


withMonitor :: Environment a -> Environment ()
withMonitor env = do
  i <- getInstID
  inEnvironment $ OpenAI.envMonitorStart i (m i)
  _ <- env
  inEnvironment $ OpenAI.envMonitorClose i
  where
    m :: InstID -> OpenAI.Monitor
    m (InstID t) = OpenAI.Monitor ("/tmp/"<> T.pack (show CartPoleV0) <>"-" <> t) True False False


toState :: MonadThrow m => Reward -> Value -> m (Obs Reward StateCP)
toState r o =
  case (fromJSON o :: Result StateCP) of
    Error str -> throw $ UnexpectedServerResponse str
    Success o -> return $ Next r o


inEnvironment :: ClientM a -> Environment a
inEnvironment = Environment . lift


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance MonadEnv Environment StateCP Action Reward where
  reset :: Environment (Initial StateCP)
  reset = do
    i <- getInstID
    Observation o <- inEnvironment . OpenAI.envReset $ i
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
    out <- inEnvironment . OpenAI.envStep i $ renderStep mon
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

  -- | no reward function is needed when interacting with the OpenAI gym
  -- reward :: Action -> Environment Reward
  -- reward _ = return 0

  -- | no action needs to be run when interacting with the OpenAI gym
  -- runAction :: Action -> Environment ()
  -- runAction _ = return ()



-------------------------------------------------------------------------------
-- |
-- Module    :  Environments.Gym.Internal
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Underlying implementation to run a Gym environment using the
-- @gym-http-client@.
-------------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Environments.Gym.Internal
  ( GymEnvironmentT(..)
  , RunnerT
  , Runner
  , GymException(..)
  , runEnvironmentT
  , runEnvironment
  , runDefaultEnvironmentT
  , runDefaultEnvironment
  , _reset
  , _step
  ) where

import Control.MonadEnv
import Reinforce.Prelude
import Data.Aeson
import Control.MonadMWCRandom
import Control.Monad.Except
import qualified Data.Text as T (pack)
import qualified OpenAI.Gym as OpenAI
import Servant.Client
import Data.Event (Event)
import qualified Data.Event as Event
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import OpenAI.Gym
  ( GymEnv(..)
  , InstID(..)
  , Observation(..)
  )

-- | A newtype wrapper around Servant so that 'GymEnvironmentT' doesn't require
-- Servant to be at the bottom of the stack
newtype ClientT t a
  = ClientT (ReaderT ClientEnv (ExceptT ServantError t) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ServantError
    , MonadReader ClientEnv
    , MonadIO
    , MonadThrow
    )


instance MonadTrans ClientT where
   lift = ClientT . lift . lift

-- | runner of a ClientT, which should really be in Servant, but since it's not
-- there we will roll our own. This allows us to have other things at the botton
-- of our transformer stack
runClientT :: MonadIO t => ClientT t a -> ClientEnv -> t (Either ServantError a)
runClientT (ClientT m) env = runExceptT $ runReaderT m env

-- | Lift a ClientM action into ClientT
liftClientM :: MonadIO t => ClientM a -> ClientT t a
liftClientM m = ClientT . ReaderT $ \env -> ExceptT $ liftIO (runClientM m env)


-- | The gym environment, which is shared through all openai/gym instances
newtype GymEnvironmentT s a t x = GymEnvironmentT
  { getEnvironmentT :: RWST GymConfigs (DList (Event Reward s a)) (LastState s) (ClientT t) x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader GymConfigs
    , MonadWriter (DList (Event Reward s a))
    , MonadState (LastState s)
    , MonadRWS GymConfigs (DList (Event Reward s a)) (LastState s)
    )

instance MonadTrans (GymEnvironmentT s a) where
   lift = GymEnvironmentT . lift . lift

-- | Use IO as the base monad in the transformer stack.
type GymEnvironment s a = GymEnvironmentT s a IO

instance (MonadIO t, MonadMWCRandom t) => MonadMWCRandom (GymEnvironmentT s a t) where
  getGen = liftIO getGen

-- | run a servant action in 'GymEnvironmentT'
inEnvironment :: MonadIO t => ClientM x -> GymEnvironmentT s a t x
inEnvironment c = GymEnvironmentT $ lift $ liftClientM c

-- | type alias for a runEnvironment type which executes an environment action
type RunnerT s a t x = Bool -> GymEnvironmentT s a t x -> t (Either ServantError (DList (Event Reward s a)))

-- | type alias of 'RunnerT' in IO
type Runner s a x = RunnerT s a IO x

-- ========================================================================= --

-- | Configurations for an environment
-- TODO: there are more of these in the gym documentation
data GymConfigs
  = GymConfigs InstID Bool    -- ^ the instance id, as well as a flag of if we want to render the state

-- | Remember the last state of the episode
data LastState o
  = LastState Integer o       -- ^ the episode number and last state
  | Uninitialized Integer     -- ^ a flag that the state is no longer initialized, and the current episode
  deriving (Eq, Show)


-- | Possible errors we might encounter while interacting with our environment
data GymException
  = UnexpectedServerResponse String
  | TypeError String
  | EnvironmentRequiresReset
  deriving (Show)

instance Exception GymException where


-- | run an effectful gym to completion and return either an error, or the history of the agent.
-- FIXME: move this into a History monad seperate from the Environment so that we don't blow up memory.
runEnvironmentT
  :: forall o a t x . MonadIO t
  => GymEnv
  -> Manager
  -> BaseUrl
  -> Bool
  -> GymEnvironmentT o a t x
  -> t (Either ServantError (DList (Event Reward o a)))
runEnvironmentT t m u mon env = runClientT action (ClientEnv m u)
  where
    action :: ClientT t (DList (Event Reward o a))
    action = do
      i <- liftClientM $ OpenAI.envCreate t
      (_, w) <- execRWST (getEnvironmentT renderableEnv) (GymConfigs i mon) (Uninitialized 0)
      liftClientM $ OpenAI.envClose i
      return w

    renderableEnv :: GymEnvironmentT o a t ()
    renderableEnv =
      if mon
      then withMonitor env
      else void env

-- | same as 'runEnvironmentT', but with IO as the base monad
runEnvironment
  :: GymEnv
  -> Manager
  -> BaseUrl
  -> Bool
  -> GymEnvironment o a x
  -> IO (Either ServantError (DList (Event Reward o a)))
runEnvironment = runEnvironmentT


-- | same as 'runEnvironmentT', however use http-client's default manager settings
runDefaultEnvironmentT
  :: MonadIO t
  => GymEnv
  -> Bool
  -> GymEnvironmentT o a t x
  -> t (Either ServantError (DList (Event Reward o a)))
runDefaultEnvironmentT t m e = do
  mngr <- liftIO (newManager defaultManagerSettings)
  runEnvironmentT t mngr (BaseUrl Http "localhost" 5000 "") m e


-- | same as 'runEnvironment', however use http-client's default manager settings
runDefaultEnvironment
  :: GymEnv
  -> Bool
  -> GymEnvironment o a x
  -> IO (Either ServantError (DList (Event Reward o a)))
runDefaultEnvironment = runDefaultEnvironmentT


-- | get the environment's id from the gym server
getInstID :: Monad t => GymEnvironmentT o a t InstID
getInstID = ask >>= \(GymConfigs i _) -> pure i


-- | if a user wants to render the agent as it performs, this makes sure to properly
-- start and stop the monitor
withMonitor :: MonadIO t => GymEnvironmentT o a t x -> GymEnvironmentT o a t ()
withMonitor env = do
  i <- getInstID
  inEnvironment $ OpenAI.envMonitorStart i (m i)
  _ <- env
  inEnvironment $ OpenAI.envMonitorClose i
  return ()
  where
    m :: InstID -> OpenAI.Monitor
    m (InstID t) = OpenAI.Monitor ("/tmp/"<> T.pack (show CartPoleV0) <>"-" <> t) True False False


-- | ensure that the gym has reset before stepping, otherwise throw 'EnvironmentRequiresReset'
stepCheck :: MonadThrow t => GymEnvironmentT o a t ()
stepCheck =
  get >>= \case
    Uninitialized _ -> throwM EnvironmentRequiresReset
    LastState _ _   -> return ()


-- | generic rest function which makes a call to the gym and returns the first observation
_reset :: (MonadIO t, MonadThrow t, FromJSON o) => GymEnvironmentT o a t (Initial o)
_reset = do
  i <- getInstID
  Observation o <- inEnvironment . OpenAI.envReset $ i
  s <- aesonToState o
  get >>= \case
    Uninitialized ep -> put $ LastState (ep+1) s
    LastState   ep _ -> put $ LastState (ep+1) s
  return $ Initial s

-- | generic step function which takes a ToJSONable action and returns a reward
-- and a FromJSONable state
_step
  :: (MonadIO t, MonadThrow t, ToJSON a, r ~ Reward, FromJSON o)
  => a -> GymEnvironmentT o a t (Obs r o)
_step a = do
  stepCheck
  GymConfigs i mon <- ask
  out <- inEnvironment . OpenAI.envStep i $ renderStep mon
  let r = OpenAI.reward out
  if OpenAI.done out
  then do
    s <- aesonToMaybeState (OpenAI.observation out)
    LastState ep prior <- get
    tell . pure $ Event.Event ep r prior a
    return $ Done r s
  else do
    s <- aesonToState (OpenAI.observation out)
    let n = Next r s
    LastState ep prior <- get
    put $ LastState ep s
    tell . pure $ Event.Event ep r prior a
    return n
  where
    renderStep :: Bool -> OpenAI.Step
    renderStep = OpenAI.Step (toJSON a)

-- | Convert an aeson value into an environment's state
aesonToState :: forall o m . (FromJSON o, MonadThrow m) => Value -> m o
aesonToState = aesonToMaybeState >=> \case
  Nothing -> throw $ UnexpectedServerResponse "observation returned was null"
  Just o -> pure o


-- | Convert an aeson value into an environment's state, but safer
aesonToMaybeState :: forall o m . (FromJSON o, MonadThrow m) => Value -> m (Maybe o)
aesonToMaybeState Null = pure Nothing
aesonToMaybeState    o =
  case (fromJSON o :: Result o) of
    Error  str -> throw $ UnexpectedServerResponse str
    Success o' -> pure  $ Just o'


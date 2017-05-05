{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Environments.Gym.Internal where

import Control.MonadEnv
import Reinforce.Prelude
import Data.DList
import Data.Logger
import Data.Aeson
import Control.MonadMWCRandom
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


newtype GymEnvironment s a x = GymEnvironment
  { getEnvironment :: RWST GymConfigs (DList (Event Reward s a)) (LastState s) ClientM x }
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
    , Logger
    )

instance MonadMWCRandom (GymEnvironment s a) where
  getGen = liftIO getGen

inEnvironment :: ClientM x -> GymEnvironment s a x
inEnvironment c = GymEnvironment $ lift c


type Runner s a x = Bool -> GymEnvironment s a x -> IO (Either ServantError (DList (Event Reward s a)))

-- ========================================================================= --

data GymConfigs
  = GymConfigs InstID Bool    -- ^ the instance id, as well as a flag of if we want to render the state


data LastState o
  = LastState Integer o       -- ^ the episode number and last state
  | Uninitialized Integer     -- ^ a flag that the state is no longer initialized, and the current episode
  deriving (Eq, Show)


data GymException
  = UnexpectedServerResponse [Char]
  | TypeError [Char]
  | EnvironmentRequiresReset
  deriving (Show)

instance Exception GymException where


runEnvironment :: forall o a x . GymEnv -> Manager -> BaseUrl -> Bool -> GymEnvironment o a x -> IO (Either ServantError (DList (Event Reward o a)))
runEnvironment t m u mon env = runClientM action (ClientEnv m u)
  where
  action :: ClientM (DList (Event Reward o a))
  action = do
    i <- OpenAI.envCreate t
    (_, w) <- execRWST (getEnvironment renderableEnv) (GymConfigs i mon) (Uninitialized 0)
    OpenAI.envClose i
    return w

  renderableEnv :: GymEnvironment o a ()
  renderableEnv =
    if mon
    then withMonitor env
    else env >> return ()


runDefaultEnvironment :: GymEnv -> Bool -> GymEnvironment o a x -> IO (Either ServantError (DList (Event Reward o a)))
runDefaultEnvironment t m e = do
  mngr <- newManager defaultManagerSettings
  runEnvironment t mngr (BaseUrl Http "localhost" 5000 "") m e


getInstID :: GymEnvironment o a InstID
getInstID = ask >>= \(GymConfigs i _) -> return i


withMonitor :: GymEnvironment o a x -> GymEnvironment o a ()
withMonitor env = do
  i <- getInstID
  inEnvironment $ OpenAI.envMonitorStart i (m i)
  _ <- env
  inEnvironment $ OpenAI.envMonitorClose i
  return ()
  where
    m :: InstID -> OpenAI.Monitor
    m (InstID t) = OpenAI.Monitor ("/tmp/"<> T.pack (show CartPoleV0) <>"-" <> t) True False False


stepCheck :: GymEnvironment o a ()
stepCheck =
  get >>= \case
    Uninitialized _ -> throwM EnvironmentRequiresReset
    LastState _ _   -> return ()


_reset :: FromJSON o => GymEnvironment o a (Initial o)
_reset = do
  i <- getInstID
  Observation o <- inEnvironment . OpenAI.envReset $ i
  s <- aesonToState o
  get >>= \case
    Uninitialized ep -> put $ LastState (ep+1) s
    LastState   ep _ -> put $ LastState (ep+1) s
  return $ Initial s


_step :: (ToJSON a, r ~ Reward, FromJSON o) => a -> GymEnvironment o a (Obs r o)
_step a = do
  stepCheck
  GymConfigs i mon <- ask
  out <- inEnvironment . OpenAI.envStep i $ renderStep mon
  if OpenAI.done out
  then return $ Done (OpenAI.reward out)
  else do
    s <- aesonToState (OpenAI.observation out)
    let r = OpenAI.reward out
        n = Next r s

    LastState ep prior <- get
    put $ LastState ep s
    tell . pure  $ Logger.Event ep r prior a
    return n
  where
    renderStep :: Bool -> OpenAI.Step
    renderStep = OpenAI.Step (toJSON a)


aesonToState :: forall o m . (FromJSON o, MonadThrow m) => Value -> m o
aesonToState o =
  case (fromJSON o :: Result o) of
    Error str -> throw $ UnexpectedServerResponse str
    Success o -> return o

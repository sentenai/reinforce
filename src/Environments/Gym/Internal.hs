{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Environments.Gym.Internal where

import Control.MonadEnv
import Reinforce.Prelude
import Data.DList
import Data.Logger
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


class GymEnvironment m o a r | m -> o a r where
  inEnvironment :: ClientM x -> m x
  getEnvironment :: m x -> RWST GymConfigs (DList (Event r o a)) (LastState o) ClientM x


-- FIXME: chop this down
type GymContext m o a r =
  ( MonadIO m
  , MonadThrow m
  , Num r
  , MonadWriter (DList (Event r o a)) m
  , MonadReader GymConfigs m
  , MonadState (LastState o) m
  , MonadRWS GymConfigs (DList (Event r o a)) (LastState o) m
  , GymEnvironment m o a r
  , Logger m
  )

runEnvironment :: forall m o a r x . GymContext m o a r => GymEnv -> Manager -> BaseUrl -> Bool -> m x -> IO (Either ServantError (DList (Event r o a)))
runEnvironment t m u mon env = runClientM action (ClientEnv m u)
  where
  action :: ClientM (DList (Event r o a))
  action = do
    i <- OpenAI.envCreate t
    (_, w) <- execRWST (getEnvironment renderableEnv) (GymConfigs i mon) (Uninitialized 0)
    OpenAI.envClose i
    return w

  renderableEnv :: m ()
  renderableEnv =
    if mon
    then withMonitor env
    else env >> return ()

runDefaultEnvironment
  :: forall m o a r x . GymContext m o a r
  => GymEnv -> Bool -> m x -> IO (Either ServantError (DList (Event r o a)))
runDefaultEnvironment t m e = do
  mngr <- newManager defaultManagerSettings
  runEnvironment t mngr (BaseUrl Http "localhost" 5000 "") m e


getInstID :: MonadReader GymConfigs m => m InstID
getInstID = ask >>= \(GymConfigs i _) -> return i


withMonitor :: (GymEnvironment m o a r, MonadReader GymConfigs m) => m x -> m ()
withMonitor env = do
  i <- getInstID
  inEnvironment $ OpenAI.envMonitorStart i (m i)
  _ <- env
  inEnvironment $ OpenAI.envMonitorClose i
  return ()
  where
    m :: InstID -> OpenAI.Monitor
    m (InstID t) = OpenAI.Monitor ("/tmp/"<> T.pack (show CartPoleV0) <>"-" <> t) True False False

stepCheck :: (MonadThrow m, MonadState (LastState o) m) => m ()
stepCheck =
  get >>= \case
    Uninitialized _ -> throwM EnvironmentRequiresReset
    LastState _ _   -> return ()


_reset :: (GymContext m o a r, FromJSON o) => m (Initial o)
_reset = do
  i <- getInstID
  Observation o <- inEnvironment . OpenAI.envReset $ i
  s <- aesonToState o
  get >>= \case
    Uninitialized ep -> put $ LastState (ep+1) s
    LastState   ep _ -> put $ LastState (ep+1) s
  return $ Initial s

_step :: (GymContext m o a r, ToJSON a, r ~ Reward, FromJSON o) => a -> m (Obs r o)
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



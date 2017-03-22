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
module Environments.CartPoleV0 where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import qualified Spaces.State as Spaces
import Data.DList

import Data.Aeson
import System.Process
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.Text as T (pack)
import qualified OpenAI.Gym as OpenAI
import Control.Exception (AssertionFailed(..))
import Servant.Client
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
    )

data GymConfigs = GymConfigs InstID Bool

type EpNum = Integer

type LastState = (EpNum, StateCP)

data Event = Event EpNum StateCP Action Double
  deriving Show


data GymException
  = UnexpectedServerResponse [Char]
  | TypeError [Char]
  deriving (Show)


instance Exception GymException where


data Action = GoLeft | GoRight
  deriving (Show, Eq, Enum, Bounded)


-- | CartPoleV0 has an action space of "discrete 2" containing {0..n-1}
instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON GoLeft  = toJSON (0 :: Int)
  toJSON GoRight = toJSON (1 :: Int)


data StateCP = StateCP
  { position  :: Float  -- ^ position of the cart on the track
  , angle     :: Float  -- ^ angle of the pole with the vertical
  , velocity  :: Float  -- ^ cart velocity
  , angleRate :: Float  -- ^ rate of change of the angle
  } deriving (Show, Eq)


instance Spaces.StateSpace StateCP where
  toVector (StateCP p a v r) = Spaces.toVector [p, a, v, r]
  fromVector vec =
    case getVals of
      Nothing -> throw $ AssertionFailed "malformed vector found"
      Just s -> return s

    where
      getVals :: Maybe StateCP
      getVals = StateCP
        <$> findField 0
        <*> findField 1
        <*> findField 2
        <*> findField 3

      findField :: Int -> Maybe Float
      findField i = double2Float <$> vec V.!? i


instance Monoid StateCP where
  mempty :: StateCP
  mempty = StateCP 0 0 0 0

  -- TODO: maybe this is something we can use uniplate for
  mappend :: StateCP -> StateCP -> StateCP
  (StateCP p0 a0 v0 r0) `mappend` (StateCP p1 a1 v1 r1) =
    StateCP (p0+p1) (a0+a1) (v0+v1) (r0+r1)


instance FromJSON StateCP where
  parseJSON :: Value -> Parser StateCP
  parseJSON arr@(Array _)= do
    (p, a, v, r) <- parseJSON arr :: Parser (Float, Float, Float, Float)
    return $ StateCP p a v r
  parseJSON invalid    = typeMismatch "StateCP" invalid


type ObsCP = Obs Reward StateCP


instance MonadEnv Environment StateCP Action Reward where
  reset :: Environment ObsCP
  reset = do
    i <- getInstID
    Observation o <- inEnvironment . OpenAI.envReset $ i
    n@(Next _ s) <- toState 0 o
    (ep, _) <- get
    put (ep+1, s)
    return n

  step :: Action -> Reward -> Environment ObsCP
  step a _ = do
    GymConfigs i mon <- Environment ask
    out <- inEnvironment . OpenAI.envStep i $ renderStep mon
    if OpenAI.done out
    then return Done
    else do
      n@(Next r s) <- toState (OpenAI.reward out) (OpenAI.observation out)
      (ep, prior) <- get
      put (ep, s)
      tell . pure  $ Event ep prior a r
      return n
    where
      renderStep :: Bool -> OpenAI.Step
      renderStep = OpenAI.Step (toJSON a)

  -- | no reward function is needed when interacting with the OpenAI gym
  reward :: Action -> Environment Reward
  reward _ = return 0

  -- | no action needs to be run when interacting with the OpenAI gym
  runAction :: Action -> Environment ()
  runAction _ = return ()


toState :: MonadThrow m => Reward -> Value -> m ObsCP
toState r o =
  case (fromJSON o :: Result StateCP) of
    Error str -> throw $ UnexpectedServerResponse str
    Success o -> return $ Next r o


inEnvironment :: ClientM a -> Environment a
inEnvironment = Environment . lift


runEnvironment :: Manager -> BaseUrl -> Bool -> Environment a -> IO (Either ServantError (DList Event))
runEnvironment m u mon env = runClientM action (ClientEnv m u)
  where
  action :: ClientM (DList Event)
  action = do
    i <- OpenAI.envCreate CartPoleV0
    (_, w) <- execRWST (getEnvironment renderableEnv) (GymConfigs i mon) (0, mempty)
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


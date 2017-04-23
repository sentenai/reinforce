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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Environments.Gym.FrozenLakeV0 where

import Reinforce.Prelude
import Control.MonadEnv.Internal
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

data StateFL = StateFL

instance FromJSON StateFL where
  parseJSON :: Value -> Parser StateFL
  parseJSON arr@(Array _)= do
    (p, a, v, r) <- parseJSON arr :: Parser (Float, Float, Float, Float)
    return $ StateFL
  parseJSON invalid    = typeMismatch "StateFL" invalid

data Action = Up | Down | Left | Right
  deriving (Enum, Bounded, Ord, Show, Eq)

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = toJSON . fromEnum

type Event = Logger.Event Reward StateFL Action

newtype Environment a = Environment { getEnvironment :: RWST GymConfigs (DList Event) (LastState StateFL) ClientM a }
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


instance I.GymEnvironment Environment StateFL Action Reward where
  inEnvironment  = Environment . lift
  getEnvironment = getEnvironment


toState :: MonadThrow m => Reward -> Value -> m (Obs Reward StateFL)
toState r o =
  case (fromJSON o :: Result StateFL) of
    Error str -> throw $ UnexpectedServerResponse str
    Success o -> return $ Next r o


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance MonadEnv Environment StateFL Action Reward where
  reset :: Environment (Obs Reward StateFL)
  reset = do
    i <- getInstID
    Observation o <- inEnvironment . OpenAI.envReset $ i
    n@(Next _ s) <- toState 0 o
    get >>= \case
      Uninitialized ep -> put $ LastState (ep+1) s
      LastState   ep _ -> put $ LastState (ep+1) s
    return n

  step :: Action -> Environment (Obs Reward StateFL)
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



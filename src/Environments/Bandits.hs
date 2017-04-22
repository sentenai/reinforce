{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Environments.Bandits where

import Control.MonadEnv.Internal hiding (Reward)
import Control.MonadMWCRandom
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.DList
import Data.Maybe
import qualified Data.Logger as Logger
import qualified System.Random.MWC as MWC
import Control.Exception.Safe (assert)
import Spaces.Shared

import Reinforce.Prelude

data Config = Config
  { nBandits :: Int -- | only 10 arms for the time being
  , offset   :: Int
  , stdDev   :: Float
  , bandits  :: Vector NormalDistribution
  , gen      :: GenIO
  }

type Event = Logger.Event Reward () Action

type Reward = Double

newtype Action = Action { unAction :: Int }
  deriving (Eq, Ord, Show, Enum, Generic)

instance Bounded Action where
  minBound = Action 0
  maxBound = Action 9

instance Hashable Action where

mkAction :: Int -> Environment Action
mkAction i = Environment $ do
  n <- nBandits <$> ask
  assert (i > n || i < 0) (pure $ Action i)

newtype Environment a = Environment { getEnvironment :: RWST Config (DList Event) () IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader Config
    , MonadWriter (DList Event)
    , MonadState ()
    , MonadRWS Config (DList Event) ()
    )

runEnvironment :: Config -> Environment () -> IO (DList Event)
runEnvironment c (Environment m) = do
  g <- MWC.createSystemRandom
  snd <$> evalRWST m c ()

defaultBandits :: GenIO -> Config
defaultBandits = mkBandits 10 2 0.1

mkBandits :: Int -> Int -> Float -> GenIO -> Config
mkBandits n offset std = Config n offset std $
  V.fromList $ fmap (`rewardDist` std) [offset .. offset + n - 1]
  where
    rewardDist :: Int -> Float -> NormalDistribution
    rewardDist m s = normalDistr (fromIntegral m) (realToFrac s)

instance MonadMWCRandom Environment where
  getGen = Environment $ fmap gen ask

instance MonadEnv Environment () Action Reward where
  -- this isn't an episodic environment... we'll have to split this out later
  reset :: Environment (Initial ())
  reset = return $ Initial ()

  step :: Action -> Environment (Obs Reward ())
  step (Action a) = do
    rwd <- genContVar =<< (! a) . bandits <$> ask
    tell . pure $ Logger.Event 0 rwd () (Action a)
    return $ Next rwd ()

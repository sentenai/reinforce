{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Environments.Bandits where

import Control.MonadEnv.Internal hiding (Reward)
import Control.MonadMWCRandom
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.DList
import Data.Maybe
import qualified Data.Logger as Logger
import qualified System.Random.MWC as MWC

import Reinforce.Prelude

data Config = Config
  { nBandits :: Int
  , offset   :: Int
  , stdDev   :: Float
  , bandits  :: Vector NormalDistribution
  , gen      :: GenIO
  }

type Reward = Double
type Action = Int
type Event = Logger.Event Reward () Action

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

runEnvironment :: Environment () -> Config -> IO (DList Event)
runEnvironment (Environment m) c = do
  g <- MWC.createSystemRandom
  snd <$> evalRWST m c ()

defaultBandits :: Int -> GenIO -> Config
defaultBandits n = mkBandits n 2 0.5

mkBandits :: Int -> Int -> Float -> GenIO -> Config
mkBandits n offset std = Config n offset std $
  V.fromList $ fmap (`rewardDist` std) [offset .. offset + n - 1]
  where
    rewardDist :: Int -> Float -> NormalDistribution
    rewardDist m s = normalDistr (fromIntegral m) (realToFrac s)

instance MonadMWCRandom Environment where
  getGen = Environment $ ask >>= return . gen

instance MonadEnv Environment () Action Reward where
  -- this isn't an episodic environment... we'll have to split this out later
  reset :: Environment (Obs Reward ())
  reset = return $ Next 0 ()

  step :: Action -> Environment (Obs Reward ())
  step a = do
    rwd <- genContVar =<< (! a) . bandits <$> ask
    tell . pure $ Logger.Event 0 rwd () a
    return $ Next rwd ()

reward :: Action -> Environment Reward
reward a = genContVar =<< (! a) . bandits <$> ask

  -- runAction :: Action -> Environment ()
  -- runAction _ = return ()


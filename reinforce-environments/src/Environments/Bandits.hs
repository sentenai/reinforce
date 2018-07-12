-------------------------------------------------------------------------------
-- |
-- Module    :  Environments.Bandits
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Implementation of an n-armed bandit environment.
--
-- FIXME: currently this is only for a 10-armed bandit. This needs to be tied
-- to a config.
-------------------------------------------------------------------------------
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Environments.Bandits
  ( Environment(..)
  , runEnvironment
  , Event.Event(..)
  , Config
  , Action
  , mkBandits
  , defaultBandits
  , mkAction
  ) where

import Control.MonadEnv
import Control.MonadMWCRandom
import qualified Data.Vector as V
import Data.Vector ((!), Vector)
import Data.Hashable
import qualified Data.Event as Event
import Control.Exception.Safe (assert, MonadThrow)
import qualified Statistics.Distribution as Dist
import Statistics.Distribution.Normal
import Data.DList
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Trans.RWS (RWST, evalRWST)
import GHC.Generics


-- | FIXME: only 10 arms for the time being. This is where a "discrete space"
-- would be nice
data Config = Config
  { nBandits :: Int
  , offset   :: Int
  , stdDev   :: Float
  , bandits  :: Vector NormalDistribution
  , gen      :: GenIO
  }

instance Show Config where
  show c = "Config" ++
    "{nBandits="++ show (nBandits c)++
    "{offset="++ show (offset c)++
    "{bandit_stdDevs="++ show (stdDev c)++
    "{bandit_means="++ show (fmap Dist.mean . V.toList $ bandits c)++
    "}"

type Event = Event.Event Reward () Action

-- | The slot machine index whose arm will be pulled
newtype Action = Action { unAction :: Int }
  deriving (Eq, Ord, Show, Enum, Generic)

instance Bounded Action where
  minBound = Action 0
  maxBound = Action 9

instance Hashable Action where

-- | Convert an Int to an Action  in the bandit environment. Throw if the Int
-- falls out of bounds.
mkAction :: Int -> Environment Action
mkAction i = Environment $ do
  n <- nBandits <$> ask
  assert (i > n || i < 0) (pure $ Action i)

-- | Monad for an n-armed bandit environment
newtype Environment a = Environment
  { getEnvironment :: RWST Config (DList Event) () IO a }
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

-- | run an n-armed bandit environment
runEnvironment :: Config -> Environment () -> IO (DList Event)
runEnvironment c (Environment m) = snd <$> evalRWST m c ()

-- | Give the default config of a 10-armed bandit
defaultBandits :: GenIO -> Config
defaultBandits = mkBandits 10 2 0.1

-- | helper function to build a bandits config with normally-distributed
-- reward functions
mkBandits :: Int -> Int -> Float -> GenIO -> Config
mkBandits n offset' std = Config n offset' std $
  V.fromList $ fmap (`rewardDist` std) [offset' .. offset' + n - 1]
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
    tell . pure $ Event.Event 0 rwd () (Action a)
    return $ Next rwd ()

-------------------------------------------------------------------------------
-- |
-- Module    :  Classifiers.RL.Control.MonadMWCRandom
-- Copyright :  (c) Sentenai 2017
-- License   :  Proprietary
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- typeclass to remove extraneous mwc-random functions
-------------------------------------------------------------------------------
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.MonadMWCRandom
  ( MonadMWCRandom(..)
  , MonadMWCRandomIO
  , MWCRand
  , MWCRandT(..)
  , runMWCRand
  , runMWCRandT
  -- * re-exports from System.Random.MWC
  , GenIO
  -- * wrappers for System.Random.MWC
  , uniform
  , uniformR
  -- * wrappers for Statistics.Distribution
  , Control.MonadMWCRandom.genContVar
  -- * extras
  , sampleFrom
  ) where

import Reinforce.Prelude
import qualified System.Random.MWC as MWC
import qualified Statistics.Distribution as Stats
import Control.MonadEnv.Internal (MonadEnv(..), Obs)


-- | MonadMWCRandom for public use. FIXME: use with PrimState so that we can use ST
class Monad m => MonadMWCRandom m where
  getGen :: m GenIO

-- | A convenience type constraint with MonadMWCRandom and MonadIO.
type MonadMWCRandomIO m = (MonadIO m, MonadMWCRandom m)

-------------------------------------------------------------------------------

instance MonadMWCRandom m => MonadMWCRandom (StateT s m) where
  getGen :: StateT s m GenIO
  getGen = lift getGen


instance MonadMWCRandom m => MonadMWCRandom (ReaderT s m) where
  getGen :: ReaderT s m GenIO
  getGen = lift getGen


-- | in the end, we can always use IO to get our generator, but we will get a
-- new generator every time.
instance MonadMWCRandom IO where
  getGen :: IO GenIO
  getGen = MWC.createSystemRandom


-------------------------------------------------------------------------------

uniform :: (MonadIO m, MonadMWCRandom m, Variate a) => m a
uniform = getGen >>= liftIO . MWC.uniform


uniformR :: (MonadIO m, MonadMWCRandom m, Variate a) => (a, a) -> m a
uniformR r = getGen >>= liftIO . MWC.uniformR r


genContVar :: (MonadIO m, MonadMWCRandom m, Stats.ContGen d) => d -> m Double
genContVar d = getGen >>= liftIO . Stats.genContVar d


-- ========================================================================= --
-- * Utility functions functions

-- | Sample a single index from a list of weights, converting the list into
-- a distribution
sampleFrom :: (MonadIO m, MonadMWCRandom m) => [Double] -> m (Int, [Double])
sampleFrom xs = uniform >>= return . choose >>= return . (,dist)
  where
    dist :: [Double]
    dist = map (/ total) xs

    total :: Double
    total = sum xs

    choose :: Double -> Int
    choose n =
      -- Return the head index (unsafeHead is safe since the last elem's snd must be 1.0)
      fst . unsafeHead .

      -- Drop while the cumulative sum is < the given value
      dropWhile ((< n) . snd) .

      -- Pair each elem with its index
      zip [0..] .

      -- Transform list of probabilities to cumulative sum
      scanl1 (+) $ dist


-- ========================================================================= --
-- A concrete type for MonadMWCRandom

-- | a helper wrapper to share a generator without using reader
newtype MWCRandT m a = MWCRandT { getMWCRandT :: ReaderT GenIO m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadIO)


-- | unwrap MonadMWCRandom
runMWCRandT :: MWCRandT m a -> GenIO -> m a
runMWCRandT = runReaderT . getMWCRandT


-- | simple type alias for transformer-less variant
type MWCRand a = MWCRandT Identity a


-- | run a transformerless MWC-random Monad
runMWCRand :: MWCRand a -> GenIO -> a
runMWCRand = runMWCRand


-- | instance declaration of MonadMWCRandom for MWCRandT
instance Monad m => MonadMWCRandom (MWCRandT m) where
  getGen :: MWCRandT m GenIO
  getGen = MWCRandT ask


-- | An instance which allows for an environment to hold a reference to a shared
-- MWC-random generator
instance MonadEnv m s a r => MonadEnv (MWCRandT m) s a r where
  reset :: MWCRandT m (Obs r s)
  reset = lift reset

  step :: a -> MWCRandT m (Obs r s)
  step a = lift $ step a

  -- runAction :: a -> MWCRandT m ()
  -- runAction = lift . runAction

  -- reward :: a -> MWCRandT m r
  -- reward = lift . reward


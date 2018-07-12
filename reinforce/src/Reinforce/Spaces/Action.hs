-------------------------------------------------------------------------------
-- |
-- Module    :  Spaces.Action
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- typeclass for a discrete action space, as well as helper functions
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reinforce.Spaces.Action
  ( DiscreteActionSpace(..)
  -- , oneHot
  , oneHot'
  , allActions
  , randomChoice
  ) where

import Control.Monad.IO.Class
-- import Numeric.LinearAlgebra.Static (R)
-- import qualified Numeric.LinearAlgebra.Static as LA

import Control.MonadMWCRandom
import GHC.TypeLits
import Data.Proxy
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Mostly tags around an Enum, but includes information about the size of
-- an action space and is used in helper functions.
class (Bounded a, Enum a) => DiscreteActionSpace a where
  type Size a :: Nat

  toAction :: Int -> a
  toAction = toEnum

  fromAction :: a -> Int
  fromAction = fromEnum


-- | one-hot encode a bounded enumerable. Doesn't care if minBound is < or > 0
-- oneHot :: forall a . (KnownNat (Size a), DiscreteActionSpace a) => a -> R (Size a)
-- oneHot e = LA.vector . V.toList
--   $ V.unsafeUpd (replicateZeros (Proxy :: Proxy a)) [(fromEnum e, 1)]


-- | one-hot encode a bounded enumerable
oneHot' :: forall a . (DiscreteActionSpace a) => a -> Vector Double
oneHot' e = V.unsafeUpd (replicateZeros (Proxy :: Proxy a)) [(fromEnum e, 1)]


-- | helper function to initialize a one-hot vector
replicateZeros :: forall a . (Enum a, Bounded a) => Proxy a -> Vector Double
replicateZeros _ = V.fromList $ replicate (fromEnum (maxBound :: a) + 1) 0


-- | helper function to get all actions in a discrete action space
allActions :: DiscreteActionSpace a => [a]
allActions = [minBound..maxBound]

-- | make a uniform-random selection of an Action in a discrete action space
randomChoice
  :: forall m a . (MonadIO m , MonadMWCRandom m, DiscreteActionSpace a)
  => m a
randomChoice = toEnum . fst <$> sampleFrom uniformDist
  where
    uniformDist :: [Double]
    uniformDist = fmap (\a -> convert a / total) allActions
      where
        convert :: a -> Double
        convert = fromIntegral . fromEnum

        total :: Double
        total = sum (fmap convert allActions)




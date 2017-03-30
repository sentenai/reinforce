{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spaces.Action where

import Reinforce.Prelude
import Numeric.LinearAlgebra.Static (R)
import qualified Numeric.LinearAlgebra.Static as LA
import qualified Data.Vector as V


class (Bounded a, Enum a) => DiscreteActionSpace a where
  type Size a :: Nat

  toAction :: Int -> a
  toAction = toEnum

  fromAction :: a -> Int
  fromAction = fromEnum


-- | one-hot encode a bounded enumerable. Doesn't care if minBound is < or > 0
oneHot :: forall a . (KnownNat (Size a), DiscreteActionSpace a) => a -> R (Size a)
oneHot e = LA.vector . V.toList $ V.unsafeUpd zeros [(fromEnum e, 1)]
  where
    zeros :: Vector Double
    zeros = V.fromList $ replicate (fromEnum (maxBound :: a) + 1) 0


allActions :: (Bounded a, DiscreteActionSpace a) => [a]
allActions = [minBound..maxBound]



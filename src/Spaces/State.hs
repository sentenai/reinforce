{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spaces.State where

import Reinforce.Prelude
import qualified Data.Vector as V
import Numeric.LinearAlgebra.Static


class StateSpaceStatic s where
  type Size s :: Nat
  toR   :: s -> R (Size s)
  -- fromR :: MonadThrow m => R (Size s) -> m s

instance StateSpaceStatic () where
  type Size () = 0
  toR = const $ vector []
  -- fromR _ = return ()

-- ========================================================================= --

class StateSpace s where
  toVector   :: s -> Vector Double
  fromVector :: MonadThrow m => Vector Double -> m s


instance StateSpace (Vector Double) where
  toVector   = id
  fromVector = return . id

instance Integral n => StateSpace (Vector n) where
  toVector   = fmap fromIntegral
  fromVector = return . fmap round

instance StateSpace [Float] where
  toVector   = V.fromList . fmap realToFrac
  fromVector = return . fmap double2Float . V.toList

-- ========================================================================= --

-- | one-hot encode a bounded enumerable
oneHot :: forall a . (Bounded a, Enum a) => a -> Vector Double
oneHot e = V.unsafeUpd zeros [(fromEnum e, 1)]
  where
    zeros :: Vector Double
    zeros = V.fromList $ replicate (fromEnum (maxBound :: a) + 1) 0



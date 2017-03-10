{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spaces.State where

import Reinforce.Prelude
import qualified Data.Vector as V

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
oneHot :: forall n e . (Num n, Bounded e, Enum e) => e -> Vector n
oneHot e = V.unsafeUpd zeros [(fromEnum e, 1)]
  where
    zeros :: Vector n
    zeros = V.fromList $ replicate (fromEnum (maxBound :: e)) 0

-------------------------------------------------------------------------------
-- |
-- Module    :  Spaces.State
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reinforce.Spaces.State
  ( StateSpace(..)
  , StateSpaceStatic(..)
  ) where

import Reinforce.Prelude
import qualified Data.Vector as V
import Numeric.LinearAlgebra.Static


-- | State space information to convert to and from a static hmatrix vector
class StateSpaceStatic s where
  type Size s :: Nat
  toR   :: s -> R (Size s)
  -- fromR :: MonadThrow m => R (Size s) -> m s

instance StateSpaceStatic () where
  type Size () = 0
  toR = const $ vector []
  -- fromR _ = return ()

-- ========================================================================= --

-- | State space information to convert to and from a Data.Vector
class StateSpace s where
  toVector   :: s -> Vector Double
  fromVector :: MonadThrow m => Vector Double -> m s


instance StateSpace (Vector Double) where
  toVector   = id
  fromVector = pure

instance Integral n => StateSpace (Vector n) where
  toVector   = fmap fromIntegral
  fromVector = return . fmap round

instance StateSpace [Float] where
  toVector   = V.fromList . fmap realToFrac
  fromVector = return . fmap double2Float . V.toList

-- ========================================================================= --



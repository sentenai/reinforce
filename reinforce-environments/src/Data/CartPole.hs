-------------------------------------------------------------------------------
-- |
-- Module    :  Data.CartPole
-- Copyright :  (c) Sentenai 2017
-- License   :  BSD3
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Shared datatypes between Gym environments and the haskell implementation of
-- CartPole.
-------------------------------------------------------------------------------
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Data.CartPole
  ( StateCP(..)
  , Action(..)
  , Event
  ) where


import qualified Reinforce.Spaces.State as Spaces

import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import GHC.Generics
import Control.Exception (AssertionFailed(..), throw)
import Reinforce.Spaces
import Reinforce.Spaces.Action (Size)
-- import Numeric.LinearAlgebra.Static
import qualified Data.Logger as Logger
import qualified Data.Vector as V

-- | Specific datatype for a CartPole event
type Event = Logger.Event Double StateCP Action

-- | Cartpole can only go left or right has an action space
-- of "discrete 2" containing {0..n-1}.
--
-- FIXME: Migrate this to either a more generic "directions" actions
-- (would need things like "up", "down" versions as well) or a "discrete
-- actions" version. I'm a fan of the former.
data Action
  = GoLeft
  | GoRight
  deriving (Show, Eq, Enum, Bounded, Ord, Generic)

instance Hashable Action

instance DiscreteActionSpace Action where
  type Size Action = 2

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON GoLeft  = toJSON (0 :: Int)
  toJSON GoRight = toJSON (1 :: Int)


-- | The state of a cart on a pole in a CartPole environment
data StateCP = StateCP
  { position  :: Float  -- ^ position of the cart on the track
  , angle     :: Float  -- ^ angle of the pole with the vertical
  , velocity  :: Float  -- ^ cart velocity
  , angleRate :: Float  -- ^ rate of change of the angle
  } deriving (Show, Eq, Generic, Ord)

instance Hashable StateCP

instance Semigroup StateCP where
  (StateCP a0 b0 c0 d0) <> (StateCP a1 b1 c1 d1)
    = StateCP (a0+a1) (b0+b1) (c0+c1) (d0+d1)

instance Monoid StateCP where
  mempty = StateCP 0 0 0 0
  mappend = (<>)

instance Spaces.StateSpace StateCP where
  toVector (StateCP p a v r) = Spaces.toVector [p, a, v, r]
  fromVector vec =
    case getVals of
      Nothing -> throw $ AssertionFailed "malformed vector found"
      Just s -> return s

    where
      getVals :: Maybe StateCP
      getVals = StateCP
        <$> findField 0
        <*> findField 1
        <*> findField 2
        <*> findField 3

      findField :: Int -> Maybe Float
      findField i = realToFrac <$> vec V.!? i

instance FromJSON StateCP where
  parseJSON :: Value -> Parser StateCP
  parseJSON arr@(Array _)= do
    (p, a, v, r) <- parseJSON arr :: Parser (Float, Float, Float, Float)
    return $ StateCP p a v r
  parseJSON invalid    = typeMismatch "StateCP" invalid

instance StateSpaceStatic StateCP where
  type Size StateCP = 4
  -- toR = vector . V.toList . Spaces.toVector
  -- fromR = Spaces.fromVector . V.fromList . LA.toList . unwrap



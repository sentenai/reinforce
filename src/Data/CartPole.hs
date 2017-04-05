{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
module Data.CartPole where

import Reinforce.Prelude
import qualified Spaces.State as Spaces

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import Control.Exception (AssertionFailed(..))
import Spaces.Action
import Spaces.State
import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Logger as Logger

type Event = Logger.Event Double StateCP Action

data Action = GoLeft | GoRight
  deriving (Show, Eq, Enum, Bounded)

instance DiscreteActionSpace Action where
  type Size Action = 2

-- | CartPole has an action space of "discrete 2" containing {0..n-1}
instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON GoLeft  = toJSON (0 :: Int)
  toJSON GoRight = toJSON (1 :: Int)


data StateCP = StateCP
  { position  :: Float  -- ^ position of the cart on the track
  , angle     :: Float  -- ^ angle of the pole with the vertical
  , velocity  :: Float  -- ^ cart velocity
  , angleRate :: Float  -- ^ rate of change of the angle
  } deriving (Show, Eq)

instance Monoid StateCP where
  mempty = StateCP 0 0 0 0
  -- FIXME : remove this

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
      findField i = double2Float <$> vec V.!? i

instance FromJSON StateCP where
  parseJSON :: Value -> Parser StateCP
  parseJSON arr@(Array _)= do
    (p, a, v, r) <- parseJSON arr :: Parser (Float, Float, Float, Float)
    return $ StateCP p a v r
  parseJSON invalid    = typeMismatch "StateCP" invalid

instance StateSpaceStatic StateCP where
  type Size StateCP = 4
  toR = vector . V.toList . Spaces.toVector
  fromR = Spaces.fromVector . V.fromList . LA.toList . unwrap



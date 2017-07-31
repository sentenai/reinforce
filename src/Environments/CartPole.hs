-------------------------------------------------------------------------------
-- |
-- Module    :  Environments.CartPole
-- Copyright :  (c) Sentenai 2017
-- License   :  Proprietary
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- * CartPole by Sutton et al.
--
-- Taken from https://webdocs.cs.ualberta.ca/~sutton/book/code/pole.c
-- with some added insights from the OpenAI gym
--
-- cart_and_pole: the cart and pole dynamics; given action and current state,
-- estimates next state
--
-- cart_pole:  Takes an action (0 or 1) and the current values of the
-- four state variables and updates their values by estimating the state
-- TAU seconds later.
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Environments.CartPole
  ( Environment(..)
  , runEnvironmentWithSeed
  , runEnvironmentWithSeed_
  , runEnvironment
  , runEnvironment_
  , cartPoleConf
  , Logger.Event(..)
  , Action
  , StateCP
  ) where

import Control.MonadEnv
import Control.MonadMWCRandom
import Data.DList
import Data.Maybe
import qualified System.Random.MWC as MWC

import Reinforce.Prelude
import Data.CartPole
import qualified Data.Logger as Logger


newtype Environment a = Environment
  { getEnvironment :: RWST CartPoleConf (DList Event) CartPoleState IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadReader CartPoleConf
    , MonadWriter (DList Event)
    , MonadState CartPoleState
    , MonadRWS CartPoleConf (DList Event) CartPoleState
    )

runEnvironmentWithSeed :: Environment () -> GenIO -> IO (DList Event)
runEnvironmentWithSeed (Environment m) g =
  snd <$> evalRWST m (cartPoleConf g) initialCartPoleState

runEnvironmentWithSeed_ :: Environment () -> GenIO -> IO ()
runEnvironmentWithSeed_ a b = runEnvironmentWithSeed a b >> pure ()

runEnvironment :: Environment () -> IO (DList Event)
runEnvironment m = MWC.createSystemRandom >>= runEnvironmentWithSeed m

runEnvironment_ :: Environment () -> IO ()
runEnvironment_ m = runEnvironment m >> pure ()


data CartPoleConf = CartPoleConf
  { gravity    :: Float
  , masscart   :: Float
  , masspole   :: Float
  , poleLength :: Float -- ^ actually half the pole's length
  , forceMag   :: Float
  , tau        :: Float -- ^ seconds between state updates
  , gen        :: GenIO
  }

data CartPoleState = CartPoleState
  { epNum           :: Integer
  , done            :: Bool
  , current         :: StateCP
  , stepsBeyondDone :: Maybe Int
  } deriving (Show, Eq)

polemassLength :: CartPoleConf -> Float
polemassLength s = masspole s * poleLength s

totalMass :: CartPoleConf -> Float
totalMass s = masspole s + masscart s

cartPoleConf :: GenIO -> CartPoleConf
cartPoleConf g = CartPoleConf
  { gravity = 9.8
  , masscart = 1.0
  , masspole = 0.1
  , poleLength = 0.5
  , forceMag = 10.0
  , tau = 0.02
  , gen = g
  }

initialCartPoleState :: CartPoleState
initialCartPoleState = CartPoleState
  { epNum = 0
  , done = True
  , current = StateCP 0 0 0 0
  , stepsBeyondDone = Nothing
  }


-- | Angle at which to fail the episode
thetaThresholdRadians :: Float
thetaThresholdRadians = 12 * 2 * pi / 360


xThreshold :: Float
xThreshold = 2.4


hasFallen :: StateCP -> Bool
hasFallen s
  =  position s < (-1 * xThreshold)
  || position s > xThreshold
  ||    angle s < (-1 * thetaThresholdRadians)
  ||    angle s > thetaThresholdRadians


-- Angle limit set to 2 * thetaThresholdRadians so failing observation is still within bounds
high :: StateCP
high = StateCP
  { position  = xThreshold * 2
  , angle     = thetaThresholdRadians * 2
  , velocity  = 500 -- maxBound
  , angleRate = 500 -- maxBound
  }


instance MonadMWCRandom Environment where
  getGen = Environment $ gen <$> ask


uniformRandStateCP :: (MonadIO m, MonadMWCRandom m) => m StateCP
uniformRandStateCP
  = StateCP
  <$> uniformR (-0.05, 0.05)
  <*> uniformR (-0.05, 0.05)
  <*> uniformR (-0.05, 0.05)
  <*> uniformR (-0.05, 0.05)


instance MonadEnv Environment StateCP Action Reward where
  reset :: Environment (Initial StateCP)
  reset = do
    s <- uniformRandStateCP
    CartPoleState epN _ _ st <- get
    put $ CartPoleState (epN+1) False s st
    return $ Initial s

  step :: Action -> Environment (Obs Reward StateCP)
  step a = do
    conf <- ask
    CartPoleState epN _ s st <- get

    let x     = position s
        xDot  = velocity s
        theta = angle    s
        thetaDot = angleRate s

    let force    = (if a == GoLeft then -1 else 1) * forceMag conf
        costheta = cos theta
        sintheta = sin theta

    let temp     = (force + polemassLength conf * (thetaDot ** 2) * sintheta) / totalMass conf
        thetaacc = (gravity conf * sintheta - costheta * temp)
                   / (poleLength conf * (4 / 3 - masspole conf * (costheta ** 2) / totalMass conf))
        xacc     = temp - polemassLength conf * thetaacc * costheta / totalMass conf

    let next = StateCP
          { position  = x        + tau conf * xDot
          , velocity  = xDot     + tau conf * xacc
          , angle     = theta    + tau conf * thetaDot
          , angleRate = thetaDot + tau conf * thetaacc
          }

    let fallen = hasFallen next
        putNextState ss = do
          put $ CartPoleState epN fallen next ss
          tell . pure $ Logger.Event epN (maybe 0 (fromIntegral . fromEnum . (> 0)) ss) s a

    if not fallen
    then putNextState st >> return (Next 1 next)
    else case st of
      Nothing ->            putNextState (Just 0)     >> return (Done 1 Nothing)  -- pole just fell!
      Just 0  -> warning >> putNextState (Just 1)     >> return (Done 0 Nothing)
      Just nsteps -> putNextState (Just $ nsteps + 1) >> return (Done 0 Nothing)

    where
      warning :: MonadIO io => io ()
      warning = liftIO . print $
        "You are calling step even though this environment has already returned done = True."
        ++ "You should always call 'reset()' once you receive 'done = True' -- any further steps are undefined behavior."



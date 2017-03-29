-- ========================================================================= --
-- CartPole by Sutton et al.
-- Taken from https://webdocs.cs.ualberta.ca/~sutton/book/code/pole.c
-- with some added insights from the OpenAI gym
-- ========================================================================= --
-- cart_and_pole: the cart and pole dynamics; given action and current state, estimates next state
--
-- cart_pole:  Takes an action (0 or 1) and the current values of the
-- four state variables and updates their values by estimating the state
-- TAU seconds later.
--
--        get_box:           The cart-pole's state space is divided into 162
--                           boxes.  get_box returns the index of the box into
--                           which the current state appears.
-- ----------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Environments.CartPole where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import Control.MonadMWCRandom
import Data.DList
import Data.Maybe
import qualified System.Random.MWC as MWC
import Environments.CartPoleV0 (StateCP(..), Action(..), Event)


newtype Environment a = Environment { getEnvironment :: RWST CartPoleConf (DList Event) CartPoleState IO a }
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

runEnvironment :: Environment () -> IO (DList Event)
runEnvironment m = MWC.createSystemRandom >>= runEnvironmentWithSeed m


data CartPoleConf = CartPoleConf
  { gravity     :: Float
  , masscart    :: Float
  , masspole    :: Float
  , pole_length :: Float -- ^ actually half the pole's length
  , force_mag   :: Float
  , tau         :: Float -- ^ seconds between state updates
  , gen         :: GenIO
  }

data CartPoleState = CartPoleState
  { epNum           :: Integer
  , done            :: Bool
  , current         :: StateCP
  , stepsBeyondDone :: Maybe Int
  } deriving (Show, Eq)

polemass_length :: CartPoleConf -> Float
polemass_length s = masspole s * pole_length s

total_mass :: CartPoleConf -> Float
total_mass s = masspole s + masscart s

cartPoleConf :: GenIO -> CartPoleConf
cartPoleConf g = CartPoleConf
  { gravity = 9.8
  , masscart = 1.0
  , masspole = 0.1
  , pole_length = 0.5
  , force_mag = 10.0
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
theta_threshold_radians :: Float
theta_threshold_radians = 12 * 2 * pi / 360


x_threshold :: Float
x_threshold = 2.4


hasFallen :: StateCP -> Bool
hasFallen s = (position s) < (-1 * x_threshold)
  || (position s) > x_threshold
  || (angle s) < (-1 * theta_threshold_radians)
  || (angle s) > theta_threshold_radians


-- Angle limit set to 2 * theta_threshold_radians so failing observation is still within bounds
high :: StateCP
high = StateCP
  { position  = x_threshold * 2
  , angle     = theta_threshold_radians * 2
  , velocity  = 500 -- maxBound
  , angleRate = 500 -- maxBound
  }


instance MonadMWCRandom Environment where
  getGen = Environment $ ask >>= return . gen


uniformRandStateCP :: (MonadIO m, MonadMWCRandom m) => m StateCP
uniformRandStateCP
  = StateCP
  <$> uniformR (-0.05, 0.05)
  <*> uniformR (-0.05, 0.05)
  <*> uniformR (-0.05, 0.05)
  <*> uniformR (-0.05, 0.05)


instance MonadEnv Environment StateCP Action Reward where
  reset :: Environment (Obs Reward StateCP)
  reset = do
    s <- uniformRandStateCP
    CartPoleState epN _ _ st <- get
    put $ CartPoleState (epN+1) False s st
    return $ Next 0 s

  step :: Action -> Reward -> Environment (Obs Reward StateCP)
  step a _ = do
    conf <- ask
    CartPoleState epN _ s st <- get

    let x     = position s
        x_dot = velocity s
        theta = angle    s
        theta_dot = angleRate s

    let force    = (if a == GoLeft then -1 else 1) * force_mag conf
        costheta = cos theta
        sintheta = sin theta

    let temp     = (force + polemass_length conf * (theta_dot ** 2) * sintheta) / total_mass conf
        thetaacc = (gravity conf * sintheta - costheta * temp)
                   / (pole_length conf * (4 / 3 - masspole conf * (costheta ** 2) / total_mass conf))
        xacc     = temp - polemass_length conf * thetaacc * costheta / total_mass conf

    let next = StateCP
          { position  = x         + tau conf * x_dot
          , velocity  = x_dot     + tau conf * xacc
          , angle     = theta     + tau conf * theta_dot
          , angleRate = theta_dot + tau conf * thetaacc
          }

    let fallen = hasFallen next
        putNextState = put . CartPoleState epN fallen next

    if not fallen
    then putNextState st >> return (Next 1 next)
    else case st of
      Nothing ->            putNextState (Just 0)     >> return (Done 1)  -- pole just fell!
      Just 0  -> warning >> putNextState (Just 1)     >> return (Done 0)
      Just nsteps -> putNextState (Just $ nsteps + 1) >> return (Done 0)

    where
      warning :: MonadIO io => io ()
      warning = liftIO . print $
        "You are calling step even though this environment has already returned done = True."
        ++ "You should always call 'reset()' once you receive 'done = True' -- any further steps are undefined behavior."

  -- | no reward function is needed when interacting with the OpenAI gym
  reward :: Action -> Environment Reward
  reward _ = return 0

  -- | no action needs to be run when interacting with the OpenAI gym
  runAction :: Action -> Environment ()
  runAction _ = return ()




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
-- ----------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Environments.CartPole where

import Control.MonadEnv.Internal
import Control.MonadMWCRandom
import Data.DList
import Data.Maybe
import qualified System.Random.MWC as MWC
import SDL.Video

import Reinforce.Prelude
import Data.CartPole
import qualified Data.Logger as Logger


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
        putNextState ss = do
          put $ CartPoleState epN fallen next ss
          tell . pure $ Logger.Event epN (if (maybe 0 id ss) > 0 then 1 else 0) s a

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

RenderModes = Human | RGBArray
videoFPS = 50

_render :: RenderModes -> Maybe Viewer -> Bool -> Int -> m (Maybe Viewer)
_render m v close fps = do
  if close && isJust v
  then closeViewer (fromJust v) >> return (Nothing)
  else if (isNothing v)
    then
      -- from gym.envs.classic_control import rendering
      -- self.viewer = rendering.Viewer(screen_width, screen_height)
      -- l,r,t,b = -cartwidth/2, cartwidth/2, cartheight/2, -cartheight/2
      -- axleoffset =cartheight/4.0
      -- cart = rendering.FilledPolygon([(l,b), (l,t), (r,t), (r,b)])
      -- self.carttrans = rendering.Transform()
      -- cart.add_attr(self.carttrans)
      -- self.viewer.add_geom(cart)
      -- l,r,t,b = -polewidth/2,polewidth/2,polelen-polewidth/2,-polewidth/2
      -- pole = rendering.FilledPolygon([(l,b), (l,t), (r,t), (r,b)])
      -- pole.set_color(.8,.6,.4)
      -- self.poletrans = rendering.Transform(translation=(0, axleoffset))
      -- pole.add_attr(self.poletrans)
      -- pole.add_attr(self.carttrans)
      -- self.viewer.add_geom(pole)
      -- self.axle = rendering.make_circle(polewidth/2)
      -- self.axle.add_attr(self.poletrans)
      -- self.axle.add_attr(self.carttrans)
      -- self.axle.set_color(.5,.5,.8)
      -- self.viewer.add_geom(self.axle)
      -- self.track = rendering.Line((0,carty), (screen_width,carty))
      -- self.track.set_color(0,0,0)
      -- self.viewer.add_geom(self.track)
      -- if self.state is None: return None
      undefined
    else
      undefined

--         x = self.state
--         cartx = x[0]*scale+screen_width/2.0 # MIDDLE OF CART
--         self.carttrans.set_translation(cartx, carty)
--         self.poletrans.set_rotation(-x[2])

--         return self.viewer.render(return_rgb_array = mode=='rgb_array')
  where
    screen_width = 600
    screen_height = 400

    world_width = x_threshold * 2
    scale = screen_width / world_width
    carty = 100 -- TOP OF CART
    polewidth = 10.0
    polelen = scale * 1.0
    cartwidth = 50.0
    cartheight = 30.0




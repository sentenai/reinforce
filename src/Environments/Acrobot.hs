-------------------------------------------------------------------------------
-- |
-- Module     : Environments.Acrobot
-- Copyright  : Copyright 2013, RLPy http://acl.mit.edu/RLPy
-- License    : BSD3
-- Author     : Christoph Dann <cdann@cdann.de>
-- Maintainer : sam@stites.io
-- Stability  : experimental
-- Portability: non-portable
-- Credits    : OpenAI/gym, Alborz Geramifard, Robert H. Klein, Christoph Dann,
--              William Dabney, Jonathan P. How
-- Original   : https://github.com/rlpy/rlpy/blob/master/rlpy/Domains/Acrobot.py
--
-- Acrobot is a 2-link pendulum with only the second joint actuated
-- Intitially, both links point downwards. The goal is to swing the
-- end-effector at a height at least the length of one link above the base.
-- Both links can swing freely and can pass by each other, i.e., they don't
-- collide when they have the same angle.
--
-- **STATE:**
-- The state consists of the two rotational joint angles and their velocities
-- [theta1 theta2 thetaDot1 thetaDot2]. An angle of 0 corresponds to corresponds
-- to the respective link pointing downwards (angles are in world coordinates).
--
-- **ACTIONS:**
-- The action is either applying +1, 0 or -1 torque on the joint between
-- the two pendulum links.
--
-- .. note::
--     The dynamics equations were missing some terms in the NIPS paper which
--     are present in the book. R. Sutton confirmed in personal correspondance
--     that the experimental results shown in the paper and the book were
--     generated with the equations shown in the book.
--     However, there is the option to run the domain with the paper equations
--     by setting book_or_nips = 'nips'
--
-- **REFERENCE:**
--
-- .. seealso::
--     R. Sutton: Generalization in Reinforcement Learning:
--     Successful Examples Using Sparse Coarse Coding (NIPS 1996)
--
-- .. seealso::
--     R. Sutton and A. G. Barto:
--     Reinforcement learning: An introduction.
--     Cambridge: MIT press, 1998.
--
-- .. warning::
--     This version of the domain uses the Runge-Kutta method for integrating
--     the system dynamics and is more realistic, but also considerably harder
--     than the original version which employs Euler integration,
--     see the AcrobotLegacy class.<Paste>
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Environments.Acrobot where

import Reinforce.Prelude
import Control.MonadMWCRandom

dt = 0.2

link_length_1 = 1     -- : [m]
link_length_2 = 1     -- : [m]
link_mass_1 = 1       -- : [kg] mass of link 1
link_mass_2 = 1       -- : [kg] mass of link 2
link_com_pos_1 = 0.5  -- : [m] position of the center of mass of link 1
link_com_pos_2 = 0.5  -- : [m] position of the center of mass of link 2
link_moi = 1          -- : moments of inertia for both links

max_vel_1 = 4 * pi
max_vel_2 = 9 * pi

avail_torque = [-1, 0, 1]

torque_noise_max = 0


-- : use dynamics equations from the nips paper or the book
book_or_nips = "book"
action_arrow = Nothing
domain_fig = Nothing
actions_num = 3

data ActorbotObs = ActorbotObs
  { theta1 :: Float
  , theta2 :: Float
  , thetaDot1 :: Float
  , thetaDot2 :: Float
  , vel1 :: Float
  , vel2 :: Float
  } deriving (Eq, Ord, Show)

data ActorbotState = ActorbotState
  { th1 :: Float
  , th2 :: Float
  , v1 :: Float
  , v2 :: Float
  } deriving (Eq, Ord, Show)


data Action = NegTorque | NoTorque | PosTorque

actionValue :: Action -> Float
actionValue = \case
  NegTorque -> -1
  NoTorque  ->  0
  PosTorque ->  1

high = ActorbotObs 1 1 1 1 max_vel_1  max_vel_2
low =  ActorbotObs (-1) (-1) (-1) (-1) (-max_vel_1) (-max_vel_2)

type ActorbotC m = (MonadIO m, MonadMWCRandom m, MonadState ActorbotState m)

_reset :: ActorbotC m => m ActorbotObs
_reset = do
  s <- uniformRandState
  put s
  return $ _get_ob s

_get_ob :: ActorbotState -> ActorbotObs
_get_ob (ActorbotState s0 s1 s2 s3) =
  ActorbotObs (cos s0) (sin s0) (cos s1) (sin s1) s2 s3

_terminal :: ActorbotState -> Bool
_terminal (ActorbotState s0 s1 s2 s3) = (-1 * cos s0) - (cos (s1 + s0)) > 1


_step :: MonadState undefined m => s -> Action -> m ActorbotObs
_step s (actionValue->torque) = do
  tmx <- undefined -- torque_noise_max <$> get
  noise <- uniformR (-torque_noise_max, torque_noise_max)
  -- Add noise to the force action
  let torque = torque + (if tmx > 0 then noise else 0)

  -- Now, augment the state with our force action so it can be passed to _dsdt
  let s_augmented = undefined -- np.append(s, torque)

  let ns = undefined -- rk4(self._dsdt, s_augmented, [0, self.dt])
  -- only care about final timestep of integration returned by integrator

  let ns = undefined -- last ns -- ns[-1]
  let ns = undefined -- init ns -- ns[:4]  -- omit action
  -- ODEINT IS TOO SLOW!
  -- ns_continuous = integrate.odeint(self._dsdt, self.s_continuous, [0, self.dt])
  -- self.s_continuous = ns_continuous[-1] # We only care about the state
  -- at the ''final timestep'', self.dt

  -- let ns[0] = wrap(ns[0], -pi, pi)
  -- let ns[1] = wrap(ns[1], -pi, pi)
  -- let ns[2] = bound(ns[2], -self.MAX_VEL_1, self.MAX_VEL_1)
  -- let ns[3] = bound(ns[3], -self.MAX_VEL_2, self.MAX_VEL_2)
  -- put ns
  -- let terminal = _terminal
  -- let reward = -1 if not terminal else 0
  -- return (_get_ob s, reward, terminal, {})
  return undefined


uniformRandState :: (MonadIO m, MonadMWCRandom m) => m ActorbotState
uniformRandState
  = ActorbotState
  <$> uniformR (-0.1, 0.1)
  <*> uniformR (-0.1, 0.1)
  <*> uniformR (-0.1, 0.1)
  <*> uniformR (-0.1, 0.1)




-- | Wraps @x@ so m <= x <= M; but unlike @bound@ which
-- truncates, @wrap@ wraps x around the coordinate system defined by m, M.
--
-- For example, m = -180, M = 180 (degrees), x = 360 --> returns 0.
wrap
  :: Float  -- ^ a scalar
  -> Float  -- ^ minimum possible value in range
  -> Float  -- ^ maximum possible value in range
  -> Float
wrap x mn mx =
  while (< mn) (+ diff)
    $ while (> mx) (subtract diff) x
  where
    diff :: Float
    diff = mx - mn

    while :: (Float -> Bool) -> (Float -> Float) -> Float -> Float
    while cond cont x = go x
      where
        go :: Float -> Float
        go x = if cond x then go (cont x) else x


-- Either have m as scalar, so bound(x,m,M) which returns m <= x <= M *OR*
-- have m as length 2 vector, bound(x,m, <IGNORED>) returns m[0] <= x <= m[1].
bound
 :: Float          -- ^ scalar
 -> (Float, Float) -- ^ boundary
 -> Float
bound x (mn, mx) = _bound x mn mn

-- bound x between min (m) and Max (M)
_bound :: Float -> Float -> Float -> Float
_bound x mn mx = min (max x mn) mx


-- Integrate 1D or ND system of ODEs using 4-th order Runge-Kutta.
-- This is a toy implementation which may be useful if you find
-- yourself stranded on a system w/o scipy.  Otherwise use
-- :func:`scipy.integrate`.
-- *y0*
--     initial state vector
-- *t*
--     sample times
-- *derivs*
--     returns the derivative of the system and has the
--     signature ``dy = derivs(yi, ti)``
-- *args*
--     additional arguments passed to the derivative function
-- *kwargs*
--     additional keyword arguments passed to the derivative function
-- Example 1 ::
--     ## 2D system
--     def derivs6(x,t):
--         d1 =  x[0] + 2*x[1]
--         d2 =  -3*x[0] + 4*x[1]
--         return (d1, d2)
--     dt = 0.0005
--     t = arange(0.0, 2.0, dt)
--     y0 = (1,2)
--     yout = rk4(derivs6, y0, t)
-- Example 2::
--     ## 1D system
--     alpha = 2
--     def derivs(x,t):
--         return -alpha*x + exp(-t)
--     y0 = 1
--     yout = rk4(derivs, y0, t)
-- If you have access to scipy, you should probably be using the
-- scipy.integrate tools rather than this function.
-- rk4 derivs y0 t = --  *args, **kwargs):
--   ny :: Int
--   ny = length y0
--     try:
--         Ny = len(y0)
--     except TypeError:
--         yout = np.zeros((len(t),), np.float_)
--     else:
--         yout = np.zeros((len(t), Ny), np.float_)
--
--     yout[0] = y0
--     i = 0
--
--     for i in np.arange(len(t) - 1):
--         thist = t[i]
--         dt = t[i + 1] - thist
--         dt2 = dt / 2.0
--         y0 = yout[i]
--
--         k1 = np.asarray(derivs(y0, thist, *args, **kwargs))
--         k2 = np.asarray(derivs(y0 + dt2 * k1, thist + dt2, *args, **kwargs))
--         k3 = np.asarray(derivs(y0 + dt2 * k2, thist + dt2, *args, **kwargs))
--         k4 = np.asarray(derivs(y0 + dt * k3, thist + dt, *args, **kwargs))
--         yout[i + 1] = y0 + dt / 6.0 * (k1 + 2 * k2 + 2 * k3 + k4)
--     return yout

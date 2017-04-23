-- https://webdocs.cs.ualberta.ca/~sutton/MountainCar/MountainCar1.cp
-- and openai/gym
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Environments.MountainCar where

import Reinforce.Prelude
import Control.MonadEnv.Internal
import Control.MonadMWCRandom


min_position = -1.2
max_position = 0.6
max_speed = 0.07
goal_position = 0.5


low  = (min_position, -max_speed)
high = (max_position,  max_speed)


observation_space = (low, high)


data Action = MoveLeft | StandStill | MoveRight
  deriving (Eq, Ord, Enum, Bounded)


actionValue :: Action -> Float
actionValue a = fromIntegral (fromEnum a - 1)


newtype CarState = CarState { unState :: (Float, Float) }
  deriving (Eq, Show)


newtype Environment x = Environment { getEnvironment :: RWST () () CarState IO x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState CarState
    )


instance MonadMWCRandom Environment where
  getGen = Environment $ lift getGen


instance MonadEnv Environment CarState Action Reward where
  step :: Action -> Environment (Obs Reward CarState)
  step (actionValue -> action) = do
    s@(CarState (position, _)) <- getNewState <$> get
    put s
    return $ getObs (isDone position) (-1) s
    where
      getNewState :: CarState -> CarState
      getNewState (CarState (pos, vel)) = CarState (newPos, newVel)
        where
          velocity' :: Float
          velocity' = vel + action * 0.001 + cos (3 * pos) * (-0.0025)

          velocity :: Float
          velocity = clip velocity' (-max_speed, max_speed)

          newPos :: Float
          newPos = clip (velocity + pos) (min_position, max_position)

          newVel :: Float
          newVel =
            if newPos == min_position && velocity < 0
            then 0
            else velocity


  reset :: Environment (Obs Reward CarState)
  reset = do
    p <- uniformR (-0.6, -0.4)
    v <- uniformR (-0.6, -0.4)
    let s = CarState (p, v)
    put s
    return $ Initial s


isDone :: Float -> Bool
isDone = (>= goal_position)


getObs :: Bool -> r -> s -> Obs r s
getObs True  r _ = Done r
getObs False r s = Next r s


clip :: (Ord a, Num a) => a -> (a, a) -> a
clip x (mn, mx)
  | x < mn    = mn
  | x > mx    = mn
  | otherwise = x

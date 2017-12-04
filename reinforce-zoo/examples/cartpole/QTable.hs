module Main where

import Prelude

import Reinforce.Agents
import Reinforce.Agents.QTable
import Reinforce.Algorithms.QLearning
import Environments.Gym.ClassicControl.CartPoleV0

main :: IO ()
main = do
  x <- runDefaultEnvironment False $
         runQTable defaultConfigs (Left 0.85) $
           runLearner (Just 10) (Just 10)
             rolloutQLearning
  print x


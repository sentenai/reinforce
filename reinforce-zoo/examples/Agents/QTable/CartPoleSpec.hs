module Agents.QTable.CartPoleSpec where

import Reinforce.Prelude

import Reinforce.Agents
import Reinforce.Agents.QTable
import Reinforce.Algorithms.QLearning
import Environments.Gym.ClassicControl.CartPoleV0
import Test.Hspec

main :: IO ()
main = do
  x <- runDefaultEnvironment False $
         runQTable defaultConfigs (Left 0.85) $
           runLearner (Just 10) (Just 10) $
             rolloutQLearning
  print x


spec :: Spec
spec =
  describe "running q-table over cartpole" $ do
    xit "should run and not break" $ True


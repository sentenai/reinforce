module Agents.QTable.CartPoleSpec where

import Agents.Prelude

import Agents
import Agents.QTable
import Algorithms.QLearning
import Environments.Gym.CartPoleV0
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


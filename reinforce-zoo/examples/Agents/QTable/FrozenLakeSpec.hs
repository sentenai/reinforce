module Agents.QTable.FrozenLakeSpec where

import Reinforce.Prelude

import Reinforce.Agents
import Reinforce.Agents.QTable
import Reinforce.Algorithms.QLearning
import Environments.Gym.ToyText.FrozenLakeV0 (runDefaultEnvironment)
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
  describe "running q-table over frozen lake" $ do
    xit "should run and not break" $ True


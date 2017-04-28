module Agents.QTable.FrozenLakeSpec where

import Agents.Prelude

import Agents
import Agents.QTable
import Environments.Gym.FrozenLakeV0
import Test.Hspec

main :: IO ()
main = do
  x <- runDefaultEnvironment False $
         runQTable defaultConfigs $
           runLearner (Just 10) (Just 10) $
             rolloutQLearning
  print x


spec :: Spec
spec =
  describe "running q-table over frozen lake" $ do
    xit "should run and not break" $ True


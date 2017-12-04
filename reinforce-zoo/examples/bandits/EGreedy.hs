{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude
import Reinforce.Prelude (impossible) -- same as "error", but for different semantics

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (Vector)
import System.Random.MWC (createSystemRandom)
import qualified Data.Vector as V

import Environments.Bandits    (Environment, runEnvironment, mkBandits)
import Control.MonadEnv        (Initial(..), Obs(..))
import Control.MonadMWCRandom  (uniform, uniformR)

import qualified Control.MonadEnv       as Env (step, reset)
import qualified Environments.Bandits   as Env (Config, mkAction)


nBandits :: Int
nBandits = 10

maxSteps :: Int
maxSteps = 2000

-- when to print logs about where the learner is
updateModulous :: Int
updateModulous = 50

initialBackup :: V.Vector Float
initialBackup = V.replicate nBandits 0

epsilon :: Float
epsilon = 0.1

learningRate :: Float
learningRate = 0.1

discountFactor :: Float
discountFactor = 0.1

main :: IO ()
main = do
  gen <- createSystemRandom
  let conf = mkBandits nBandits 2 0.1 gen :: Env.Config
  print "starting with bandit configs:"
  print conf

  _ <- runEnvironment conf startContinuous
  pure ()

  where
    startContinuous :: Environment ()
    startContinuous =
      Env.reset >>= \case
        EmptyEpisode -> pure ()
        Initial _    -> egreedyAgent 0 initialBackup


-- this is usually the structure of a rollout:
egreedyAgent :: Int -> Vector Float -> Environment ()
egreedyAgent stepN table
  | stepN > maxSteps = do
    printIO   ""
    printIO   "maxSteps reached"
    printIO $ "agent terminates with " ++ logMessage table

  | otherwise = do

    banditIdx <- epsilonGreedyIdx table epsilon
    bandit <- Env.mkAction banditIdx

    Env.step bandit >>= \case
      Terminated -> pure ()
      Done _ _   -> impossible "Bandit environments are continous and should never terminate"
      Next r _   -> do
        let
          oldQ = table V.! banditIdx
          updatedQ = oldQ + learningRate * (realToFrac r + discountFactor * V.maximum table - oldQ)
          updatedTable = table V.// [(banditIdx, updatedQ)]

        when (stepN `mod` updateModulous == 0 && stepN /= maxSteps) $
          printIO ("at step " ++ show stepN ++ " agent " ++ logMessage table)

        egreedyAgent (stepN+1) updatedTable


printIO :: Show x => x -> Environment ()
printIO = liftIO . print


logMessage :: Vector Float -> String
logMessage table = "maxIndex: "++ show (V.maxIndex table) ++", bandits: " ++ show table


epsilonGreedyIdx :: Vector Float -> Float -> Environment Int
epsilonGreedyIdx table eps =
  liftIO (compare eps <$> uniform) >>= \case
    GT -> pure   $ V.maxIndex table
    _  -> liftIO $ uniformR (0, length table - 1)



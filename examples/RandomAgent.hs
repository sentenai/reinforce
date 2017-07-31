module Main where

import Reinforce.Prelude
    -- ^ NoImplicitPrelude is on

import Environments.CartPole (Environment, runEnvironment_)
import Control.MonadEnv      (Initial(..), Obs(..))

import qualified Control.MonadEnv       as Env (step, reset)
import qualified Environments.CartPole  as Env (StateCP, Action)
    -- Comments:
    --     StateCP - An "observation" or "the state of the agent" - note that State overloaded, so StateCP
    --     Action  - A performable action in the environment.
import qualified Spaces.Action          as Actions (randomChoice)

main :: IO ()
main = runEnvironment_ gogoRandomAgent

  where
    gogoRandomAgent :: Environment ()
    gogoRandomAgent = forM_ [0..maxEpisodes] $ \_ ->
      Env.reset >>= \case           -- this comes from LambdaCase. Sugar for: \a -> case a of ...
        EmptyEpisode -> pure ()
        Initial obs  -> do
          liftIO . print $ "Initialized episode and am in state " ++ show obs
          rolloutEpisode obs 0

    maxEpisodes :: Int
    maxEpisodes = 100

    -- this is usually the structure of a rollout:
    rolloutEpisode :: Env.StateCP -> Double -> Environment ()
    rolloutEpisode obs totalRwd = do
      a <- liftIO Actions.randomChoice
      Env.step a >>= \case
        Done r mobs  ->
          liftIO . print
            $ "Done! final reward: " ++ show (totalRwd+r) ++ ", final state: " ++ show mobs
        Next r  obs' -> do
          liftIO . print
            $ "Stepped with " ++ show a ++ " - reward: " ++ show r ++ ", next state: " ++ show obs'
          rolloutEpisode obs' (totalRwd+r)



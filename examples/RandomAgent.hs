module Main where

import Reinforce.Prelude
import Control.MonadEnv as Env
import Control.MonadMWCRandom
import Environments.CartPole
import Spaces.Action (randomChoice)

main :: IO ()
main = runEnvironment_ $
  forM_ [0..maxEpisodes] $ \_ ->
    Env.reset >>= \case
      EmptyEpisode -> pure()
      Initial obs  -> do
        liftIO . print $ "Initialized episode and am in state " ++ show obs
        rolloutEpisode obs

  where
    maxEpisodes :: Int
    maxEpisodes = 100


    rolloutEpisode :: StateCP -> Environment ()
    rolloutEpisode obs = do
      a <- liftIO randomChoice
      Env.step a >>= \case
        Done r mobs  ->
          liftIO . print
            $ "Done! final reward: " ++ show r ++ ", final state: " ++ show mobs
        Next r  obs' -> do
          liftIO . print
            $ "Stepped with " ++ show a ++ ". reward: " ++ show r ++ ", next state: " ++ show obs'
          rolloutEpisode obs'


module Reinforce.Agents
  ( runLearner
  , clockEpisodes
  , clockSteps
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.MonadEnv
import qualified Control.MonadEnv as Env (reset)

runLearner
  :: MonadEnv m o a r
  => MonadIO m
  => Maybe Integer
  -> Maybe Integer
  -> (Maybe Integer -> o -> m ())
  -> m ()
runLearner maxEps maxSteps rollout =
  clockEpisodes maxEps 0 maxSteps rollout


clockEpisodes
  :: forall m o a r . MonadEnv m o a r
  => Maybe Integer
  -> Integer
  -> Maybe Integer
  -> (Maybe Integer -> o -> m ())
  -> m ()
clockEpisodes maxEps epn maxSteps rollout = do
  case maxEps of
    Nothing -> tick
    Just mx -> unless (epn > mx) tick
  where
    tick :: m ()
    tick = Env.reset >>= \case
      EmptyEpisode -> pure ()
      Initial s    -> do
        rollout maxSteps s
        clockEpisodes maxEps (epn+1) maxSteps rollout


clockSteps :: Monad m => Maybe Integer -> Integer -> (Integer -> m ()) -> m ()
clockSteps   Nothing st tickAct = tickAct st
clockSteps (Just mx) st tickAct = unless (st >= mx) (tickAct st)


module Reinforce.Agents where

import Reinforce.Prelude
import Control.MonadEnv

runLearner
  :: forall m o a r . MonadEnv m o a r
  => Maybe Integer
  -> Maybe Integer
  -> (Maybe Integer -> m ())
  -> m ()
runLearner maxEpisodes maxSteps rollout = clock maxEpisodes 0 goM
  where
   goM :: Integer -> m ()
   goM epn =
     rollout maxSteps
     >> clock maxEpisodes (epn + 1) goM


clock :: Monad m => Maybe Integer -> Integer -> (Integer -> m ()) -> m ()
clock   Nothing n !goM = goM n
clock (Just mx) n !goM = unless (n >= mx) (goM n)


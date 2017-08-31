module Reinforce.Algorithms.QLearning where

import Reinforce.Agents
import Reinforce.Prelude
import Control.MonadEnv (MonadEnv, Initial(..), Obs(..))
import qualified Control.MonadEnv as Env
import Reinforce.Algorithms.Internal


-- ============================================================================= --
-- | Q-Learning
-- ============================================================================= --
-- An off-Policy algorithm for TD-learning. Q-Learning learns the optimal policy
-- even when actions are selected according to a more exploratory or even random
-- policy.
--
--   Initialize Q(s, a) arbitrarily
--   For each episode:
--     Observe the initial s
--     Repeat for each step of the episode:
--       Choose a from s using policy derived from Q
--       Take action a, observe r, s'
--       Q(s, a) <- Q(s, a) + lambda * [ r + gamma * max[Q(s', a)] - Q(s, a)]
--                                                   -------------
--               estimate of optimal future value ------'
--
--       s <- s'
--     until s terminal
-- ========================================================================= --
rolloutQLearning :: forall m o a r . (MonadEnv m o a r, TDLearning m o a r, Ord r)=> Maybe Integer -> o -> m ()
rolloutQLearning maxSteps i = do
  clockSteps maxSteps 0 (goM i)
  where
    goM :: o -> Integer -> m ()
    goM s st = do
      a <- choose s
      Env.step a >>= \case
        Terminated -> return ()
        Done r ms' -> maybe (pure ()) (learn s a r) ms'
        Next r s'  -> do
          learn s a r s'
          clockSteps maxSteps (st+1) (goM s')

    learn :: o -> a -> r -> o -> m ()
    learn s a r s' = do
      lambda <- getLambda
      gamma  <- getGamma

      oldQ <- value s a
      nextQs <- sequence . fmap (value s) =<< actions s
      update s a $ oldQ + lambda * (r + gamma * (maximum nextQs) - oldQ)

rolloutEpsQLearning :: forall m o a r . (MonadEnv m o a r, TDLearning m o a r, Ord r)=> Maybe Integer -> o -> m ()
rolloutEpsQLearning maxSteps i = do
  clockSteps maxSteps 0 (goM i)
  where
    goM :: o -> Integer -> m ()
    goM s st = do
      a <- choose s
      Env.step a >>= \case
        Terminated -> return ()
        Done r ms' -> maybe (pure ()) (learn s a r) ms'
        Next r s'  -> do
          learn s a r s'
          clockSteps maxSteps (st+1) (goM s')

    learn :: o -> a -> r -> o -> m ()
    learn s a r s' = do
      lambda <- getLambda
      gamma  <- getGamma

      oldQ <- value s a
      nextQs <- sequence . fmap (value s) =<< actions s
      update s a $ oldQ + lambda * (r + gamma * (maximum nextQs) - oldQ)


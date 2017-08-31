module Reinforce.Algorithms.Sarsa where

import Reinforce.Agents
import Reinforce.Prelude
import Control.MonadEnv (MonadEnv, Obs(..))
import qualified Control.MonadEnv as Env
import Reinforce.Algorithms.Internal


-- ========================================================================= --
-- | Sarsa
-- ========================================================================= --
-- An on-policy algorithm for TD-learning. The pseudocode looks like the
-- following:
--
--   Initialize the Q-values table, Q(s, a) arbitrarily
--   For each episode:
--     Observe the initial s
--     Choose a from s using policy derived from Q (ie: eps-greedy)
--     Repeat for each step of the episode:
--       Take action a, observe r, s'
--       Choose a' from s' using policy derived from Q
--       Q(s, a) <- Q(s, a) + lambda * [ r + gamma * Q(s', a') - Q(s, a)]
--       s <- s'
--       a <- a'
--     until s terminal
-- ========================================================================= --
rolloutSarsa
  :: forall m o a r . (MonadEnv m o a r, TDLearning m o a r, Ord r)
  => MonadIO m
  => (Show a, Show o, Show r)
  => Maybe Integer
  -> o
  -> m ()
rolloutSarsa maxSteps i = do
  a <- choose i
  clockSteps maxSteps 0 (agentStep i a)

  where
    agentStep :: o -> a -> Integer -> m ()
    agentStep s a st = do
      Env.step a >>= \case
        Terminated -> return ()
        Done r ms' -> maybe (pure ()) (\s' -> choose s' >>= learn s a r s') ms'
        Next r  s' -> do
          a' <- choose s'
          learn s a r s' a'
          clockSteps maxSteps (st+1) (agentStep s' a')

    learn :: o -> a -> r -> o -> a -> m ()
    learn s a r s' a' = do
      lambda <- getLambda
      gamma  <- getGamma

      oldQ   <- value s  a
      nextQ  <- value s' a'
      update s a $ oldQ + lambda * (r + gamma * nextQ - oldQ)


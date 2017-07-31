module Algorithms.Sarsa where

import Agents
import Reinforce.Prelude
import Control.MonadEnv (MonadEnv, Initial(..), Obs(..))
import qualified Control.MonadEnv as Env
import Algorithms.Internal


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
rolloutSarsa :: forall m o a r . (MonadEnv m o a r, TDLearning m o a r, Ord r)=> Maybe Integer -> m ()
rolloutSarsa maxSteps = do
  Initial s <- Env.reset
  a <- choose s
  clock maxSteps 0 (goM s a)
  where
    goM :: o -> a -> Integer -> m ()
    goM s a st =
      Env.step a >>= \case
        Terminated -> return ()
        Done r ms' -> maybe (pure ()) (learn st s a r) ms'
        Next r s'  -> learn st s a r s'

    learn :: Integer -> o -> a -> r -> o -> m ()
    learn st s a r s' = do
      lambda <- getLambda
      gamma  <- getGamma
      a'     <- choose s'

      oldQ   <- value s  a
      nextQ  <- value s' a'
      update s a $ oldQ + lambda * (r + gamma * nextQ - oldQ)
      clock maxSteps (st+1) (goM s' a')


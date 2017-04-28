module Algorithms.Sarsa where

import Agents
import Agents.Prelude
import Control.MonadEnv (MonadEnv, Initial(..), Obs(..))
import qualified Control.MonadEnv as Env


class Monad m => Sarsa m s a r | m -> s a r where
  choose  :: s -> m a
  actions :: s -> m [a]
  update  :: s -> a -> r -> s -> a -> m ()


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
rolloutSarsa :: forall m o a r . (MonadEnv m o a r, Sarsa m o a r)=> Maybe Integer -> m ()
rolloutSarsa maxSteps = do
  Initial s <- Env.reset
  a <- choose s
  clock maxSteps 0 (goM s a)
  where
    goM :: o -> a -> Integer -> m ()
    goM s a st =
      Env.step a >>= \case
        Terminated -> return ()
        Done _     -> return ()
        Next r s'  -> do
          a' <- choose s'
          update s a r s' a'
          clock maxSteps (st + 1) (goM s' a')


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Algorithms.QLearning where

import Agents
import Agents.Prelude
import Control.MonadEnv (MonadEnv, Initial(..), Obs(..))
import qualified Control.MonadEnv as Env
import Algorithms.Internal


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
rolloutQLearning :: forall m o a r . (MonadEnv m o a r, TDLearning m o a r, Ord r)=> Maybe Integer -> m ()
rolloutQLearning maxSteps = do
  Initial s <- Env.reset
  clock maxSteps 0 (goM s)
  where
    goM :: o -> Integer -> m ()
    goM s st = do
      a <- choose s
      Env.step a >>= \case
        Terminated -> return ()
        Done _     -> return ()
        Next rwd s'  -> do
          lambda <- getLambda
          gamma  <- getGamma

          oldQ <- value s a
          nextQs <- sequence . fmap (value s) =<< actions s
          update s a $ oldQ + lambda * (rwd + gamma * (maximum nextQs) - oldQ)
          clock maxSteps (st+1) (goM s')


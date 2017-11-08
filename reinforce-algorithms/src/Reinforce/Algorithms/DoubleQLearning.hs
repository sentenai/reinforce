module Reinforce.Algorithms.DoubleQLearning where

import Reinforce.Agents
import Reinforce.Prelude
import Control.MonadEnv (MonadEnv, Obs(..))
import Control.MonadMWCRandom
import qualified Control.MonadEnv as Env
import Reinforce.Algorithms.Internal (RLParams(..))
import Reinforce.Algorithms.Double.Internal (DoubleTDLearning(..))


-- ============================================================================= --
-- | Double Q-Learning
-- ============================================================================= --
-- Double-Q-Learning avoids the maximization bias present in Q-Learning by
-- maintaining two approximate value functions which are treated completely
-- symmetrically.
--
-- The behavior policy in this case can use either both action value estimates
-- or one of them. For example it can use the sum, average, or opposite estimates.
--
-- There are also doubled versions of Sarsa and Expected Sarsa.
--
--   Initialize Q1(s, a) and Q2(s, a), for all s in S, and for all  a in A(s), arbitrarily.
--   Initialize Q1(terminal-state, ·) = Q2(terminal-state, ·) = 0
--   For each episode:
--     Observe the initial s
--     Repeat for each step of the episode:
--       Choose a from s using policy derived from Q1 and Q2 (e.g., ε-greedy in Q1 + Q2)
--       Take action a, observe r, s'
--       With 0.5 probability:
--         Q1(s, a) <- Q1(s, a) + alpha * [ r + gamma * Q2(s', argmax_a(Q1(s', a))) - Q1(s, a)]
--       Otherwise:
--         Q2(s, a) <- Q2(s, a) + alpha * [ r + gamma * Q1(s', argmax_a(Q2(s', a))) - Q2(s, a)]
--       s <- s'
--     until s terminal
-- ========================================================================= --
rolloutDoubleQLearning
  :: forall m o a r . (MonadIO m, Variate r, MonadEnv m o a r, DoubleTDLearning m o a r, Ord r, MonadMWCRandom m)
  => Maybe Integer -> o -> m ()
rolloutDoubleQLearning maxSteps i =
  clockSteps maxSteps 0 (goM i)
  where
    goM :: o -> Integer -> m ()
    goM s st = do
      a <- choose s
      Env.step a >>= \case
        Terminated -> return ()
        Done r ms' -> maybe (pure ()) (learn st s a r) ms'
        Next r s'  -> learn st s a r s'

    learn :: Integer -> o -> a -> r -> o -> m ()
    learn st s a r s' = do
      compare (0.5 :: Float) <$> uniform >>= \case
        LT -> calcQ value1 actions1 value2 s a r s' >>= update1 s a
        _  -> calcQ value2 actions2 value1 s a r s' >>= update2 s a
      clockSteps maxSteps (st+1) (goM s')


calcQ
  :: forall m o a r . (MonadEnv m o a r, DoubleTDLearning m o a r, Ord r)
  => (o -> a -> m r)
  -> (o -> m [a])
  -> (o -> a -> m r)
  -> o
  -> a
  -> r
  -> o
  -> m r
calcQ valueA actionsA valueB s a r s' = do
  lambda <- getLambda
  gamma  <- getGamma
  qA     <- valueA s a
  actsA  <- arPairs
  qB     <- valueB s' (argmaxAction actsA)
  return $ qA + lambda * (r + gamma * qB - qA)
  where
    arPairs :: m [(a, r)]
    arPairs = do
      as <- actionsA s
      rs <- traverse (valueA s) as
      return $ zip as rs

    argmaxAction :: [(a, r)] -> a
    argmaxAction acts = fst $ maximumBy (comparing snd) acts



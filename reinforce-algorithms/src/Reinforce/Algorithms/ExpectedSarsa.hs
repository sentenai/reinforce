module Reinforce.Algorithms.ExpectedSarsa where

import Reinforce.Agents
import Reinforce.Prelude
import Control.MonadEnv (MonadEnv, Obs(..))
import qualified Control.MonadEnv as Env
import Reinforce.Algorithms.Internal


-- ========================================================================= --
-- | Expected Sarsa
-- ========================================================================= --
-- A generic version of SARSA where, instead of taking the maximum (ie: how Q
-- is calculated) over the next state-action pairs, we take the expected value.
-- This changes the update rule from:
--
--     Q(s, a) <- Q(s, a) + lambda * [ r + gamma * Q(s', a') - Q(s, a)]
--
-- to:
--
--     Q(s, a) <- Q(s, a) + lambda * [ r + gamma * [Sum forall a's: P(s', a') * Q(s', a')] - Q(s, a)]
--
-- ========================================================================= --
rolloutExpectedSarsa
  :: forall m o a r . (MonadEnv m o a r, TDLearning m o a r, Fractional r, Ord r)
  => Maybe Integer
  -> o
  -> m ()
rolloutExpectedSarsa maxSteps i = do
  a <- choose i
  clockSteps maxSteps 0 (goM i a)
  where
    goM :: o -> a -> Integer -> m ()
    goM s a st =
      Env.step a >>= \case
        Terminated -> pure ()
        Done r ms' -> maybe (pure ()) (\s' -> choose s' >>= \a' -> learn s a r s' a') ms'
        Next r s'  -> do
          a' <- choose s'
          learn s a r s' a'
          clockSteps maxSteps (st+1) (goM s' a')

    learn :: o -> a -> r -> o -> a -> m ()
    learn s a r s' _ = do
      lambda <- getLambda
      gamma  <- getGamma

      -- Note that we don't hold distributions seperate from reward currently
      oldQ   <- value s a
      nextQs <- traverse (value s') =<< actions s'
      let expectedQ = sum nextQs / fromIntegral (length nextQs)
      update s a $ oldQ + lambda * (r + gamma * expectedQ - oldQ)


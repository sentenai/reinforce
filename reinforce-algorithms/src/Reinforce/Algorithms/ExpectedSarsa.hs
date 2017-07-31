module Reinforce.Algorithms.ExpectedSarsa where

import Reinforce.Agents
import Reinforce.Prelude
import Control.MonadEnv (MonadEnv, Initial(..), Obs(..))
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
  -> m ()
rolloutExpectedSarsa maxSteps = do
  Initial s <- Env.reset
  a <- choose s
  clock maxSteps 0 (goM s a)
  where
    goM :: o -> a -> Integer -> m ()
    goM s a st =
      Env.step a >>= \case
        Terminated -> pure ()
        Done r ms' -> maybe (pure ()) (learn st s a r) ms'
        Next r s'  -> learn st s a r s'

    learn :: Integer -> o -> a -> r -> o -> m ()
    learn st s a r s' = do
      lambda <- getLambda
      gamma  <- getGamma
      a'     <- choose s'

      -- Note that we don't hold distributions seperate from reward currently
      oldQ   <- value s  a
      nextQs <- sequence . map (value s') =<< actions s'
      let expectedQ = sum nextQs / (fromIntegral $ length nextQs)
      update s a $ oldQ + lambda * (r + gamma * expectedQ - oldQ)
      clock maxSteps (st+1) (goM s' a')


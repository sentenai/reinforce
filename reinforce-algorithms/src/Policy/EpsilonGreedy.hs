module Policy.EpsilonGreedy where

import Agents.Prelude
import Control.MonadMWCRandom

epsilonGreedy :: (MonadIO m, Ord r, Variate r, MonadMWCRandom m) => [(a, r)] -> r -> m a
epsilonGreedy acts = epsilonChoice (fst $ maximumBy (comparing snd) acts) acts


epsilonChoice :: (MonadIO m, Ord r, Variate r, MonadMWCRandom m) => a -> [(a, r)] -> r -> m a
epsilonChoice a acts eps = do
  compare eps <$> uniform >>= \case
    LT -> pure a
    _  -> do
      i <- uniformR (0, length acts)
      pure . fst . unsafeHead $ drop (i-1) acts



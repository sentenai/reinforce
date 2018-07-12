module Reinforce.Policy.EpsilonGreedy where

import Control.Monad.IO.Class
import Data.List (maximumBy)
import Data.Ord (comparing)

import Control.MonadMWCRandom

epsilonGreedy :: (MonadIO m, Ord r, Variate r, MonadMWCRandom m) => [(a, r)] -> r -> m a
epsilonGreedy acts = epsilonChoice (fst $ maximumBy (comparing snd) acts) acts


epsilonChoice :: (MonadIO m, Ord r, Variate r, MonadMWCRandom m) => a -> [(a, r)] -> r -> m a
epsilonChoice a acts eps = do
  compare eps <$> uniform >>= \case
    LT -> pure a
    _  -> do
      i <- uniformR (0, length acts)
      pure . fst . head $ drop (i-1) acts



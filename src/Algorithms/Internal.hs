module Algorithms.Internal where

import Agents.Prelude

class Monad m => TDLearning m s a r | m -> s a r where
  choose  :: s -> m a
  actions :: s -> m [a]
  update  :: s -> a -> r -> m ()
  value   :: s -> a -> m r

  getLambda :: m r
  getGamma :: m r



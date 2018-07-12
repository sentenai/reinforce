module Reinforce.Algorithms.Internal where

class Monad m => RLParams m r where
  getLambda :: m r
  getGamma :: m r


class RLParams m r => TDLearning m s a r | m -> s a r where
  choose  :: s -> m a
  actions :: s -> m [a]
  update  :: s -> a -> r -> m ()
  value   :: s -> a -> m r



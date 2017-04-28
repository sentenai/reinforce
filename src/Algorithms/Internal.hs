module Algorithms.Internal where

import Agents.Prelude


class Monad m => RLParams m r where
  getLambda :: m r
  getGamma :: m r


class RLParams m r => TDLearning m s a r | m -> s a r where
  choose  :: s -> m a
  actions :: s -> m [a]
  update  :: s -> a -> r -> m ()
  value   :: s -> a -> m r


class RLParams m r => DoubleTDLearning m s a r | m -> s a r where
  chooseWith :: (a, r) -> (a, r) -> m a
  choose1    :: s -> m (a, r)
  choose2    :: s -> m (a, r)
  update1    :: s -> a -> r -> m ()
  update2    :: s -> a -> r -> m ()
  actions1   :: s -> m [a]
  actions2   :: s -> m [a]
  value1     :: s -> a -> m r
  value2     :: s -> a -> m r



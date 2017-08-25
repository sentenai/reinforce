module Reinforce.Algorithms.Double.Internal where

import Reinforce.Algorithms.Internal (RLParams)

class RLParams m r => DoubleTDLearning m s a r | m -> s a r where
  choose   :: s -> m a
  update1  :: s -> a -> r -> m ()
  update2  :: s -> a -> r -> m ()
  actions1 :: s -> m [a]
  actions2 :: s -> m [a]
  value1   :: s -> a -> m r
  value2   :: s -> a -> m r



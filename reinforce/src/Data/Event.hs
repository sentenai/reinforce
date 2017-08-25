module Data.Event where

import Prelude
-- ========================================================================= --
-- | Our primary datatype for an event in a trace. Contains the episode number,
-- reward, state, and action taken (in that order).
-- TODO: change the ordering to @Event Integer s a r@
data Event r o a = Event Integer r o a
  deriving Show



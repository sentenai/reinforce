{-# LANGUAGE FlexibleInstances #-}
module Reinforce.Internal.MonadHistory where

import Reinforce.Prelude

-- Just a writer alias for the time being... which probably means I should replace it with Writer...
class (Monoid h, Monad m) => MonadHistory h m | m -> h where
  {-# MINIMAL (history | record), observe, specify #-}
  history :: (a, h) -> m a
  history (a, h) = record h >> return a

  record :: h -> m ()
  record h = history (() , h)

  observe :: m a -> m (a, w)

  specify :: m (a, w -> w) -> m a

-- instance (Monoid w, MonadWriter w m) => MonadHistory w (Writer.WriterT w m) where
--   history = Writer.writer
--   record  = Writer.tell
--   observe = Writer.listen
--   specify = Writer.pass


-- instance (Monoid w, Monad m) => MonadHistory w (RWST r w s m) where
--   history = RWS.writer
--   record  = RWS.tell
--   observe = RWS.listen
--   specify = RWS.pass



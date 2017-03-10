{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Reinforce.Prelude
  ( module X
  , whileM_
  ) where

import Control.Exception as X (assert)
import Data.List as X (intercalate)
import Data.Monoid as X
import Lens.Micro.Platform as X
import Control.Exception.Safe as X
import Prelude as X
import System.Random.MWC as X (GenIO, Variate)
import Control.Monad.RWS.Strict as X hiding ((<>))
import Control.Monad.Reader as X
import Control.Monad.State as X
import Control.Monad.Identity as X
import Control.Applicative as X
import Data.Vector as X (Vector)
import GHC.Float as X


-- | A monadic while loop
whileM_ :: forall m a . Monad m => m Bool -> m a -> m ()
whileM_ p f = go
  where
    go :: m ()
    go = p >>= decide

    decide :: Bool -> m ()
    decide True  = return ()
    decide False = f >> go


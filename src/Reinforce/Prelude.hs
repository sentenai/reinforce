{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Reinforce.Prelude
  ( module X
  , whileM_
  , head
  , unsafeHead
  ) where

import Prelude as X hiding (head)

import Control.Applicative      as X
import Control.Exception.Safe   as X
import Control.Monad.Identity   as X
import Control.Monad.Reader     as X
import Control.Monad.RWS.Strict as X hiding ((<>))
import Control.Monad.Writer.Strict as X (Writer(..))
import Control.Monad.State      as X

import Data.List   as X (intercalate)
import Data.Monoid as X
import Data.Proxy  as X (Proxy)
import Data.Text   as X (Text)
import Data.Vector as X (Vector)

import GHC.Float    as X
import GHC.Generics as X (Generic)
import GHC.TypeLits as X

import Lens.Micro.Platform as X

import Data.Hashable as X
import Statistics.Distribution.Normal as X (normalDistr, NormalDistribution)
import System.Random.MWC              as X (GenIO, Variate)
-- ========================================================================= --

import qualified Prelude as P (head)

-- | A monadic while loop
whileM_ :: forall m a . Monad m => m Bool -> m a -> m ()
whileM_ p f = go
  where
    go :: m ()
    go = p >>= decide

    decide :: Bool -> m ()
    decide True  = return ()
    decide False = f >> go

head :: [a] -> Maybe a
head [] = Nothing
head as = Just $ P.head as

unsafeHead :: [a] -> a
unsafeHead = P.head


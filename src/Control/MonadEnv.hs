-------------------------------------------------------------------------------
-- |
-- Module    :  Classifiers.RL.Control.MonadEnv
-- Copyright :  (c) Sentenai 2017
-- License   :  Proprietary
-- Maintainer:  sam@sentenai.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Public API to MonadEnv - to implement an environment, see
-- 'Classifiers.RL.Control.MonadEnv.Internal'
-------------------------------------------------------------------------------
module Control.MonadEnv
  ( MonadEnv
  , reset
  , step
  , Obs(..)
  , Initial(..)
  , Reward
  ) where

import Control.MonadEnv.Internal (Obs(..), Reward, MonadEnv, Initial)
import qualified Control.MonadEnv.Internal as I

-- | API for resetting an environment
reset :: MonadEnv e s a r => e (Initial s)
reset = I.reset

-- | API to step though an environment using an action.
step :: MonadEnv e s a r => a -> e (Obs r s)
step a = I.step a
--step a = I.reward a >>= I.step a



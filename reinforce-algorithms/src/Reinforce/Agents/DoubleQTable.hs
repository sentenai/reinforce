{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Reinforce.Agents.DoubleQTable where

import Reinforce.Prelude
import Control.MonadEnv as Env
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.MonadMWCRandom
import Data.Logger
import Reinforce.Algorithms.Internal (RLParams(..))
import Reinforce.Algorithms.Double.Internal (DoubleTDLearning(..))
import Reinforce.Policy.EpsilonGreedy

type EnvC m    = (MonadIO m, MonadMWCRandom m)
type ActionC a = (Ord a, Hashable a, Enum a, Bounded a)
type RewardC r = (Variate r, Ord r, Enum r, Num r)
type StateC o  = (Ord o, Hashable o)
type SARMap o a r = HashMap o (HashMap a r)
type SARMapLens' o a r = Lens' (DoubleQTableState o a r) (SARMap o a r)

data Configs r = Configs
  { gamma    :: r
  , epsilon  :: r
  , maxSteps :: Maybe Int
  , initialQ :: r
  , merger   :: r -> r -> r
  }


defaultConfigs :: Configs Reward
defaultConfigs = Configs 0.99 0.1 (Just 2000) 0 (+)


data DoubleQTableState o a r = DoubleQTableState
  { qs1     :: HashMap o (HashMap a r)
  , qs2     :: HashMap o (HashMap a r)
  , lambda :: Either r (Integer, r, Integer -> r -> r)
  }


qs1L    :: Lens' (DoubleQTableState o a r) (HashMap o (HashMap a r))
qs2L    :: Lens' (DoubleQTableState o a r) (HashMap o (HashMap a r))
lambdaL :: Lens' (DoubleQTableState o a r) (Either r (Integer, r, Integer -> r -> r))
qs1L    = lens qs1    $ \(DoubleQTableState _ b c) a -> DoubleQTableState a b c
qs2L    = lens qs2    $ \(DoubleQTableState a _ c) b -> DoubleQTableState a b c
lambdaL = lens lambda $ \(DoubleQTableState a b _) c -> DoubleQTableState a b c

defaultDoubleQTableState :: (StateC o, Fractional r) => Either r (r, Integer -> r -> r) -> DoubleQTableState o a r
defaultDoubleQTableState (Left i)        = DoubleQTableState mempty mempty (Left i)
defaultDoubleQTableState (Right (i, fn)) = DoubleQTableState mempty mempty (Right (0, i, fn))


newtype DoubleQTable m o a r x = DoubleQTable
  { getDoubleQTable :: RWST (Configs r) [Event r o a] (DoubleQTableState o a r) m x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Configs r)
    , MonadWriter [Event r o a]
    , MonadState (DoubleQTableState o a r)
    , MonadRWS (Configs r) [Event r o a] (DoubleQTableState o a r)
    )


runDoubleQTable
  :: (MonadEnv m o a r, StateC o, Fractional r)
  => Configs r -> Either r (r, Integer -> r -> r) -> DoubleQTable m o a r x -> m (x, DoubleQTableState o a r, [Event r o a])
runDoubleQTable conf l (DoubleQTable e) = runRWST e conf (defaultDoubleQTableState l)


instance (MonadIO m, MonadMWCRandom m) => MonadMWCRandom (DoubleQTable m o a r) where
  getGen = liftIO getGen

instance MonadEnv m o a r => MonadEnv (DoubleQTable m o a r) o a r where
  step a = DoubleQTable $ lift (step a)
  reset  = DoubleQTable $ lift reset

instance Monad m => RLParams (DoubleQTable m o a r) r where
  getLambda :: DoubleQTable m o a r r
  getLambda = use lambdaL >>= \case
    Left l -> pure l
    Right (t, l, fn) ->
      lambdaL .= Right (t+1, l', fn)
      >> pure l'
      where
        l' = fn (t+1) l

  getGamma :: DoubleQTable m o a r r
  getGamma = gamma <$> ask

instance (EnvC m, RewardC r, ActionC a, StateC o) => DoubleTDLearning (DoubleQTable m o a r) o a r where
  choose :: o -> DoubleQTable m o a r a
  choose s = do
    Configs{initialQ, merger, epsilon} <- ask
    DoubleQTableState{qs1, qs2} <- get
    let ars1 = HM.lookupDefault (initalTable initialQ) s qs1
        ars2 = HM.lookupDefault (initalTable initialQ) s qs2
        dist = HM.toList $ HM.unionWith merger ars1 ars2
        choice = fst $ maximumBy (comparing snd) dist :: a
    epsilonChoice choice dist epsilon


  update1  :: o -> a -> r -> DoubleQTable m o a r ()
  update1  = _update qs1L

  update2  :: o -> a -> r -> DoubleQTable m o a r ()
  update2  = _update qs2L

  actions1 :: o -> DoubleQTable m o a r [a]
  actions1 = _actions qs1L

  actions2 :: o -> DoubleQTable m o a r [a]
  actions2 = _actions qs2L

  value1 :: o -> a -> DoubleQTable m o a r r
  value1 = _value qs1L

  value2 :: o -> a -> DoubleQTable m o a r r
  value2 = _value qs2L


initalTable :: (ActionC a, RewardC r) => r -> HashMap a r
initalTable x = HM.fromList $ zip [minBound..maxBound] [x..]


_actions :: forall m o a r . (Monad m, StateC o, ActionC a, RewardC r) => SARMapLens' o a r -> o -> DoubleQTable m o a r [a]
_actions qsL obs = do
  Configs{initialQ} <- ask
  qs <- use qsL
  let ars = HM.lookupDefault (initalTable initialQ) obs qs
  qsL %= HM.insert obs ars  -- FIXME: this is really extra computation...
  return $ HM.keys ars


_update :: (Monad m, StateC o, ActionC a) => SARMapLens' o a r -> o -> a -> r -> DoubleQTable m o a r ()
_update qsL obs act updQ = qsL %= HM.update (Just . HM.insert act updQ) obs


_value :: (Monad m, StateC o, ActionC a) => Lens' (DoubleQTableState o a r) (SARMap o a r) -> o -> a -> DoubleQTable m o a r r
_value qsL obs act = do
  Configs{initialQ} <- ask
  qs <- use qsL
  return $ fromMaybe initialQ (qs ^. at obs . _Just ^. at act)




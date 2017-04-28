{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Agents.QTable where

import Agents.Prelude
import Agents
import Control.MonadEnv.Internal as Env
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.MonadMWCRandom
import Data.Logger


class Monad m => QLearning m s a r | m -> s a r where
  choose  :: s -> m a
  actions :: s -> m [a]
  update  :: s -> a -> r -> s -> m ()


rolloutQLearning :: forall m o a r . (MonadEnv m o a r, QLearning m o a r)=> Maybe Integer -> m ()
rolloutQLearning maxSteps = do
  Initial s <- Env.reset
  clock maxSteps 0 (goM s)
  where
    goM :: o -> Integer -> m ()
    goM s st = do
      a <- choose s
      Env.step a >>= \case
        Terminated -> return ()
        Done _     -> return ()
        Next r s'  -> do
          update s a r s'
          clock maxSteps (st+1) (goM s')


data Configs r = Configs
  { gamma    :: r
  , epsilon  :: r
  , maxSteps :: Maybe Int
  , initialQ :: r
  }


defaultConfigs :: Configs Reward
defaultConfigs = Configs 0.99 0.1 (Just 2000) 0


data QTableState o a r = QTableState
  { qs     :: HashMap o (HashMap a r)
  , lambda :: Either r (Integer, r, Integer -> r -> r)
  }


qsL :: Lens' (QTableState o a r) (HashMap o (HashMap a r))
qsL = lens qs $ \(QTableState _ b) a -> QTableState a b

lambdaL :: Lens' (QTableState o a r) (Either r (Integer, r, Integer -> r -> r))
lambdaL = lens lambda $ \(QTableState a _) b -> QTableState a b

defaultQTableState :: (Hashable o, Eq o, Fractional r) => QTableState o a r
defaultQTableState = QTableState mempty (Left 0.85)


newtype QTable m o a r x = QTable
  { getQTable :: RWST (Configs r) [Event r o a] (QTableState o a r) m x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Configs r)
    , MonadWriter [Event r o a]
    , MonadState (QTableState o a r)
    , MonadRWS (Configs r) [Event r o a] (QTableState o a r)
    )


runQTable
  :: (MonadEnv m o a r, StateC o, Fractional r)
  => Configs r -> QTable m o a r x -> m (x, QTableState o a r, [Event r o a])
runQTable conf (QTable e) = runRWST e conf defaultQTableState


instance (MonadIO m, MonadMWCRandom m) => MonadMWCRandom (QTable m o a r) where
  getGen = liftIO getGen


instance MonadEnv m o a r => MonadEnv (QTable m o a r) o a r where
  step a = QTable $ lift (step a)
  reset  = QTable $ lift reset


type EnvC m    = (MonadIO m, MonadMWCRandom m)
type ActionC a = (Ord a, Hashable a, Enum a, Bounded a)
type RewardC r = (Variate r, Ord r, Enum r, Num r)
type StateC o  = (Ord o, Hashable o)
type SARMap o a r = HashMap o (HashMap a r)


instance (EnvC m, RewardC r, ActionC a, StateC o) => QLearning (QTable m o a r) o a r where
  choose :: o -> QTable m o a r a
  choose obs = do
    Configs{epsilon} <- ask
    prob <- uniform
    acts <- actions obs
    if prob > epsilon
    then return . maximum $ acts -- greedy choice
    else do
      i <- uniformR (0, length acts)
      return . unsafeHead $ drop (i-1) acts


  actions :: o -> QTable m o a r [a]
  actions obs = do
    Configs{initialQ} <- ask
    QTableState{qs} <- get
    let ars = HM.lookupDefault (initalTable initialQ) obs qs
    qsL %= HM.insert obs ars
    return $ HM.keys ars
    where
      initalTable :: r -> HashMap a r
      initalTable x = HM.fromList $ zip [minBound..maxBound::a] [x..]


  update :: o -> a -> r -> o -> QTable m o a r ()
  update obs0 act rwd obs1 = do
    Configs{gamma, initialQ} <- ask
    QTableState{qs} <- get
    lambda <- getLambda
    as <- actions obs1
    let oldQ  = getQ qs initialQ act
        newQs = getQ qs initialQ <$> as
        updQ  = oldQ + lambda * (rwd + gamma * (maximum newQs) - oldQ)
    qsL %= setQ updQ

    where
      getQ :: SARMap o a r -> r -> a -> r
      getQ qs r a = maybe r id (qs ^. at obs0 . _Just ^. at a)

      setQ :: r -> SARMap o a r -> SARMap o a r
      setQ q ars = HM.update (Just . HM.insert act q) obs0 ars


getLambda :: Monad m => QTable m o a r r
getLambda = use lambdaL >>= \case
  Left l -> pure l
  Right (t, l, fn) ->
    lambdaL .= Right (t+1, l', fn)
    >> pure l'
    where
      l' = fn (t+1) l

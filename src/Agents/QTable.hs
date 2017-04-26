{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Agents.QTable where

import Agents.Prelude
import Agents
import Data.CartPole
import Environments.Gym.CartPoleV0
import Control.MonadEnv.Internal as Env
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.MonadMWCRandom


class Monad m => QLearning m s a | m -> s a where
  choose  :: s -> m a
  actions :: s -> m [a]
  update  :: s -> a -> Reward -> s -> m ()


rolloutQLearning
  :: Maybe Integer
  -> QTable ()
rolloutQLearning maxSteps = do
  Initial s <- Env.reset
  clock maxSteps 0 (goM s)
  where
    goM :: StateCP -> Integer -> QTable ()
    goM s st = do
      a <- choose s
      Env.step a >>= \case
        Terminated -> return ()
        Done _     -> return ()
        Next r s'  -> do
          update s a r s'
          clock maxSteps (st+1) (goM s')


data Configs = Configs
  { lambda   :: Double
  , gamma    :: Double
  , epsilon  :: Double
  , maxSteps :: Maybe Int
  , initialQ :: Reward
  }

defaultConfigs :: Configs
defaultConfigs = Configs 0.85 0.99 0.1 (Just 2000) 0


data QTableState = QTableState
  { qs :: HashMap StateCP (HashMap Action Reward)
  }

qsL :: Lens' QTableState (HashMap StateCP (HashMap Action Reward))
qsL = lens qs $ \(QTableState _) a -> QTableState a


defaultQTableState :: QTableState
defaultQTableState = QTableState $ mempty


newtype QTable x = QTable
  { getQTable :: RWST Configs [Event] QTableState Environment x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Configs
    , MonadWriter [Event]
    , MonadState QTableState
    , MonadRWS Configs [Event] QTableState
    )

runQTable :: Configs -> QTable x -> Environment (x, QTableState, [Event])
runQTable conf (QTable e) = runRWST e conf defaultQTableState

instance MonadMWCRandom QTable where
  getGen = liftIO getGen

instance MonadEnv QTable StateCP Action Reward where
  step a = QTable $ lift (step a)
  reset = QTable $ lift reset

instance QLearning QTable StateCP Action where
  choose :: StateCP -> QTable Action
  choose s = do
    Configs{epsilon} <- ask
    p <- uniform
    as <- actions s

    if p > epsilon
    then return . maximum $ as
    else do
      i <- uniformR (0, length as)
      return . unsafeHead $ drop (i-1) as
    where


  actions :: StateCP -> QTable [Action]
  actions s = do
    Configs{initialQ} <- ask
    QTableState{qs} <- get
    let ars = HM.lookupDefault (initalTable initialQ) s qs
    qsL %= HM.insert s ars
    return $ HM.keys ars
    where
      initalTable :: Reward -> HashMap Action Reward
      initalTable x = HM.fromList $ zip [minBound..maxBound::Action] [x..]


  update :: StateCP -> Action -> Reward -> StateCP -> QTable ()
  update s a r s' = do
    Configs{lambda, gamma, initialQ} <- ask
    QTableState{qs} <- get

    as <- actions s'
    let oldQ  = getQ qs initialQ a
    let newQs = getQ qs initialQ <$> as
    let updQ  = oldQ + lambda * (r + gamma * (maximum newQs) - oldQ)

    qsL %= setQ updQ
    where
      getQ :: HashMap StateCP (HashMap Action Reward) -> Reward -> Action -> Reward
      getQ qs r_ a_ =
        maybe r_ id (qs ^. at s . _Just ^. at a_)

      setQ :: Reward -> HashMap StateCP (HashMap Action Reward) -> HashMap StateCP (HashMap Action Reward)
      setQ q ars = HM.update (Just . HM.insert a q) s ars




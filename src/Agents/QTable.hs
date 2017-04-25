{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Agents.QTable where

import Agents.Prelude
import Agents
import Environments.Gym.FrozenLakeV0
import Control.MonadEnv.Internal as Env
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.MonadMWCRandom

import Numeric.LinearAlgebra.Static


class Monad m => QLearning m s a | m -> s a where
  choose  :: s -> m a
  actions :: s -> m [a]
  calcQ   :: s -> a -> m Reward
  update  :: s -> a -> Reward -> s -> m ()


rolloutQLearning
  :: Maybe Integer
  -> QTable ()
rolloutQLearning maxSteps = do
  Initial s <- Env.reset
  clock maxSteps 0 (goM s)
  where
    goM :: StateFL -> Integer -> QTable ()
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
  { qs :: HashMap StateFL (HashMap Action Reward)
  }

qsL :: Lens' QTableState (HashMap StateFL (HashMap Action Reward))
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

instance MonadMWCRandom QTable where
  getGen = liftIO getGen

instance MonadEnv QTable StateFL Action Reward where
  step a = QTable $ lift (step a)
  reset = QTable $ lift reset

instance QLearning QTable StateFL Action where
  choose :: StateFL -> QTable Action
  choose s = do
    Configs{epsilon, initialQ} <- ask
    QTableState{qs} <- get
    qsL %= HM.alter (addNew initialQ) s
    p <- uniform
    as <- actions s

    if p > epsilon
    then return . maximum $ as
    else do
      i <- uniformR (0, length as)
      return . unsafeHead $ drop (i-1) as
    where
      addNew :: Reward -> Maybe (HashMap Action Reward) -> Maybe (HashMap Action Reward)
      addNew x    Nothing = Just . initalTable $ x
      addNew _ x@(Just _) = x

      initalTable :: Reward -> HashMap Action Reward
      initalTable x = HM.fromList $ zip [minBound..maxBound::Action] [x..]


  actions :: StateFL -> QTable [Action]
  actions s = undefined


  calcQ :: StateFL -> Action -> QTable Reward
  calcQ s a = undefined


  update :: StateFL -> Action -> Reward -> StateFL -> QTable ()
  update s a r s' = do
    Configs{lambda, gamma} <- ask
    oldQ <- calcQ s a
    newQs <- sequence . map (calcQ s') =<< actions s'
    let updQ = oldQ + lambda * (r + gamma * (maximum newQs) - oldQ)
    return ()



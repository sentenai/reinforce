{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Agents.QTable where

import Agents.Prelude
import Environments.Gym.FrozenLakeV0
import Control.MonadEnv.Internal as Env
import Control.MonadMWCRandom

import Numeric.LinearAlgebra.Static


-- Just a writer alias
class (Monoid a, Monad m) => History m a where
  record   :: a -> m ()
  tabluate :: m x -> a


data Configs = Configs
  { learningRate :: Double
  , gamma        :: Double
  , eps          :: Double
  , maxSteps     :: Maybe Int
  }

defaultConfigs :: Configs
defaultConfigs = Configs 0.85 0.99 0.1 (Just 2000)


data QTableState = QTableState
  { qs :: L 4 4
  }

defaultQTableState :: QTableState
defaultQTableState = QTableState $ matrix $ replicate (4*4) 0


data LearningFunctions = LearningFunctions
  { choose  :: StateFL -> QTable Action
  , actions :: StateFL -> QTable [Action]
  , value   :: StateFL -> Action -> QTable Reward
  , update  :: StateFL -> Action -> Reward -> StateFL -> QTable ()
  }

learningFunctions :: Double -> LearningFunctions
learningFunctions eps = LearningFunctions (_choose eps) _actions _value _update


newtype QTable x = QTable
  { getQTable :: RWST LearningFunctions [Event] QTableState Environment x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader LearningFunctions

    )
instance MonadMWCRandom QTable where
  getGen = liftIO getGen

instance MonadEnv QTable StateFL Action Reward where
  step a = QTable $ lift (step a)
  reset = QTable $ lift reset

rolloutQLearning
  :: (param ~ Reward)
  => Maybe Integer
  -> param
  -> param
  -> QTable ()
rolloutQLearning maxSteps lambda gamma = do
  Initial s <- Env.reset
  clock maxSteps 0 (goM s)
  where
    goM :: StateFL -> Integer -> QTable ()
    goM s st = do
      LearningFunctions{choose, update} <- ask
      a <- choose s
      Env.step a >>= \case
  --      Terminated -> return ()
        Done _     -> return ()
        Next r s'  -> do
          update s a r s'
          clock maxSteps (st+1) (goM s')

_choose :: Double -> StateFL -> QTable Action
_choose eps s = do
  p <- uniform
  as <- _actions s

  if p > eps
  then return . maximum $ as
  else do
    i <- uniformR (0, length as)
    return . unsafeHead $ drop (i-1) as

_actions :: StateFL -> QTable [Action]
_actions s = undefined

_value :: StateFL -> Action -> QTable Reward
_value s a = undefined

_update :: StateFL -> Action -> Reward -> StateFL -> QTable ()
_update s a r s' = do
  q <- updatedQ s a r s'
  return ()

updatedQ :: StateFL -> Action -> Reward -> StateFL -> QTable Reward
updatedQ s a r s' = do
  LearningFunctions{actions, value} <- ask
  oldQ <- value s  a
  newQs <- sequence . map (value s') =<< actions s'
  let newQ = maximum newQs :: Reward
  return $ oldQ + lambda * (r + gamma * newQ - oldQ)


-- for i in range(num_episodes):
--     #Reset environment and get first new observation
--     s = env.reset()
--     rAll = 0
--     d = False
--     j = 0
--     #The Q-Table learning algorithm
--     while j < 99:
--         j+=1
--         #Choose an action by greedily (with noise) picking from Q table
--         a = np.argmax(Q[s,:] + np.random.randn(1,env.action_space.n)*(1./(i+1)))
--         #Get new state and reward from environment
--         s1,r,d,_ = env.step(a)
--         #Update Q-Table with new knowledge
--         Q[s,a] = Q[s,a] + lr*(r + y*np.max(Q[s1,:]) - Q[s,a])
--         rAll += r
--         s = s1
--         if d == True:
--             break
--     #jList.append(j)
--     rList.append(rAll)

runLearner
  :: forall m o a r . MonadEnv m o a r
  => Maybe Integer
  -> Maybe Integer
  -> r
  -> r
  -> (Maybe Integer -> r -> r -> m ())
  -> m ()
runLearner maxEpisodes maxSteps lambda gamma rollout = clock maxEpisodes 0 goM
  where
   goM :: Integer -> m ()
   goM epn = do
     rollout maxSteps lambda gamma
     clock maxEpisodes (epn + 1) goM

clock :: Monad m => Maybe Integer -> Integer -> (Integer -> m ()) -> m ()
clock   Nothing n goM = goM n
clock (Just mx) n goM = unless (n >= mx) (goM n)



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
  { lambda   :: Double
  , gamma    :: Double
  , epsilon  :: Double
  , maxSteps :: Maybe Int
  }

defaultConfigs :: Configs
defaultConfigs = Configs 0.85 0.99 0.1 (Just 2000)


data QTableState = QTableState
  { qs :: L 4 4
  }

defaultQTableState :: QTableState
defaultQTableState = QTableState $ matrix $ replicate (4*4) 0


class Monad m => QLearning m s a | m -> s a where
  choose  :: s -> m a
  actions :: s -> m [a]
  calcQ   :: s -> a -> m Reward
  update  :: s -> a -> Reward -> s -> m ()


newtype QTable x = QTable
  { getQTable :: RWST Configs [Event] QTableState Environment x }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Configs

    )
instance MonadMWCRandom QTable where
  getGen = liftIO getGen

instance MonadEnv QTable StateFL Action Reward where
  step a = QTable $ lift (step a)
  reset = QTable $ lift reset

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

instance QLearning QTable StateFL Action where
  choose :: StateFL -> QTable Action
  choose s = do
    Configs{epsilon} <- ask
    p <- uniform
    as <- actions s

    if p > epsilon
    then return . maximum $ as
    else do
      i <- uniformR (0, length as)
      return . unsafeHead $ drop (i-1) as


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
  -> (Maybe Integer -> m ())
  -> m ()
runLearner maxEpisodes maxSteps rollout = clock maxEpisodes 0 goM
  where
   goM :: Integer -> m ()
   goM epn =
     rollout maxSteps
     >> clock maxEpisodes (epn + 1) goM

clock :: Monad m => Maybe Integer -> Integer -> (Integer -> m ()) -> m ()
clock   Nothing n goM = goM n
clock (Just mx) n goM = unless (n >= mx) (goM n)



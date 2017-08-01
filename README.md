reinforce
=============
![Hackage](https://img.shields.io/hackage/v/reinforce.svg)
![Build Status](https://travis-ci.org/Sentenai/reinforce.svg?branch=master)

`reinforce` is a library which exports an OpenAI-gym-like typeclass, `MonadEnv`, with both an interface to the [`gym-http-api`][gym-http], as well as haskell-native environments which provide a substantial speed-up to the http-server interface.

This is an environment-first library, with basic reinforcment learning algorithms being developed on branches in subpackages (see [#Development and Milestones](#development-and-milestones)).
`reinforce` is currently an "alpha" release since it still needs some work defining some formal structures around what state-spaces and action-spaces should look like, however haskell's typesystem is expressive enough that this seems to be more of a "nice-to-have."

It's in active development and has some beginner-friendly contributions: from porting new gym environments to implementing new algorithms.

[gym-http]: https://github.com/openai/gym-http-api/

An example agent
=============

In `examples/`, you can find an agent which showcases some of the functionality of this library.

```haskell
module Main where

import Reinforce.Prelude
    -- ^ NoImplicitPrelude is on

import Environments.CartPole (Environment, runEnvironment_)
import Control.MonadEnv      (Initial(..), Obs(..))

import qualified Control.MonadEnv        as Env (step, reset)
import qualified Environments.CartPole   as Env (StateCP)
    -- Comments:
    --     StateCP - An "observation" or "the state of the agent" - note that State overloaded, so StateCP
    --     Action  - A performable action in the environment.
import qualified Reinforce.Spaces.Action as Actions (randomChoice)

main :: IO ()
main = runEnvironment_ gogoRandomAgent

  where
    gogoRandomAgent :: Environment ()
    gogoRandomAgent = forM_ [0..maxEpisodes] $ \_ ->
      Env.reset >>= \case           -- this comes from LambdaCase. Sugar for: \a -> case a of ...
        EmptyEpisode -> pure ()
        Initial obs  -> do
          liftIO . print $ "Initialized episode and am in state " ++ show obs
          rolloutEpisode obs 0

    maxEpisodes :: Int
    maxEpisodes = 100

    -- this is usually the structure of a rollout:
    rolloutEpisode :: Env.StateCP -> Double -> Environment ()
    rolloutEpisode obs totalRwd = do
      a <- liftIO Actions.randomChoice
      Env.step a >>= \case
        Terminated   -> pure ()
        Done r mobs  ->
          liftIO . print
            $ "Done! final reward: " ++ show (totalRwd+r) ++ ", final state: " ++ show mobs
        Next r  obs' -> do
          liftIO . print
            $ "Stepped with " ++ show a ++ " - reward: " ++ show r ++ ", next state: " ++ show obs'
          rolloutEpisode obs' (totalRwd+r)
```

You can build and run this with the following commands:

```
git clone https://github.com/Sentenai/reinforce
cd reinforce
stack build
stack exec random-agent-example
```


Note that if you want to run a gym environment, you'll have to run the [OpenAi/gym-http-api][gym-http] server with the following steps:

```
git clone https://github.com/openai/gym-http-api
cd gym-http-api
pip install -r requirements.txt
python ./gym_http_server.py
```

Currently, development has been primarily focused around classic control, so if you want to add any of the Atari environments, this would be an easy contribution!

Installing
=============

Right now, reinforce doesn't exist on hackage or stackage, so your best bet is to add this git repo to your stack.yaml file:

```yaml
packages:
- '.'
- location:
    git: git@github.com:Sentenai/reinforce.git
    commit: 'v0.0.1'
  extra-dep:true
- ...
```

and add it to your cabal file or package.yaml (recommended) dependencies.


Development and Milestones
=============

If you want to contribute, you're in luck! There are a range of things to do from the beginner haskeller to, even, advanced pythonistas!

Please [file an issue mentioning where you'd like to help](https://github.com/Sentenai/reinforce/issues), or track down @stites in the [dataHaskell gitter](https://gitter.im/dataHaskell/) or directly through [keybase.io](https://keybase.io/stites).


While you can check the [Github issues](https://github.com/Sentenai/reinforce/issues), here are some items off the top of my head which could use some immediate attention (and may also need to be filed).

A few quick environment contributions might be the following:
- (easy) add an Atari environment to the api (like pong! others might require directly commiting to `gym-http-api`)
- (med) Port Richard Sutton's Acrobot code to haskell (http://incompleteideas.net/sutton/book/code/code.html)
- (hard) break the dependency on the `OpenAI/gym-http-api` server -- this would speed up performance considerably
- (harder) render the haskell CartPole environment with SDL

Some longer-running algorithmic contributions which would take place on the `algorithms` or `deep-rl` branches might be:
- (easy) convert algorithms into agents
- (med - requires RL-foo) add a testable "convergence" criteria
- (med - requires RL-foo) Implement some eligibility trace variants to the `algorithms` branch
- (med - requires RL-foo) Add some policy gradient methods to the `algorithms` branch
- (hard - requires RL-foo) head over to the `deep-rl` branch and convert some of the deep reinforcement learning models into haskell with [tensorflow-haskell][tfhs], and/or [backprop][bp]

For a longer-term view, feel free to check out [MILESTONES.md](./MILESTONES.md).

[tfhs]:https://github.com/tensorflow/haskell
[bp]:https://github.com/mstksg/backprop


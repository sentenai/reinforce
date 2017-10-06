reinforce
=============
![Build Status](https://travis-ci.org/Sentenai/reinforce.svg?branch=master)

`reinforce` is a library which exports an openai-gym-like typeclass, `MonadEnv`, with both an interface to the [`gym-http-api`][gym-http], as well as haskell-native environments which provide a substantial speed-up to the http-server interface.

This is an environment-first library, with basic reinforcement learning algorithms being developed on branches in subpackages (see [#Development and Milestones](#development-and-milestones)).
`reinforce` is currently an "alpha" release since it still needs some work defining some formal structures around what state-spaces and action-spaces should look like, however haskell's typesystem is expressive enough that this seems to be more of a "nice-to-have".

This repo is in active development and has some beginner-friendly contributions, from porting new gym environments to implementing new algorithms. Because this library is not on hackage, if you would like to see the haddocks, [you can find it here](https://sentenai.github.io/reinforce/).

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


Note that if you want to run a gym environment, you'll have to run the [openai/gym-http-api][gym-http] server with the following steps:

```
git clone https://github.com/openai/gym-http-api
cd gym-http-api
pip install -r requirements.txt
python ./gym_http_server.py
```

Currently, development has been primarily focused around classic control, so if you want to add any of the Atari environments, this would be an easy contribution!

Installing
=============

Reinforce doesn't exist on hackage or stackage (yet), so your best bet is to add this git repo to your stack.yaml file:

```yaml
packages:
- '.'
- location:
    git: git@github.com:Sentenai/reinforce.git
    commit: 'v0.0.1'
  extra-dep:true

# This is a requirement due to some tight coupling of the gym-http-api
- location:
    git: https://github.com/stites/gym-http-api.git
    commit: '5b72789'
  subdirs:
    - binding-hs
  extra-dep: true
- ...
```

and add it to your cabal file or package.yaml (recommended) dependencies.

Architecture
=============

TODO

Development and Milestones
=============

If you want to contribute, you're in luck! There are a range of things to do from the beginner haskeller to, even, advanced pythonistas!

Please [file an issue mentioning where you'd like to help](https://github.com/Sentenai/reinforce/issues), or track down @stites in the [dataHaskell gitter](https://gitter.im/dataHaskell/) or directly through [keybase.io](https://keybase.io/stites).


While you can check the [Github issues](https://github.com/Sentenai/reinforce/issues), here are some items off the top of my head which could use some immediate attention (and may also need to be filed).

A few quick environment contributions might be the following:
- #1 (easy) - Add an Atari environment to the api (like pong! others might require directly commiting to `gym-http-api`)
- #8 (med) - Port Richard Sutton's Acrobot code to haskell
- #6 (hard) - Break the dependency on the `openai/gym-http-api` server -- this would speed up performance considerably
- #9 (harder) - Render the haskell CartPole environment with SDL

Some longer-running algorithmic contributions which would take place on the `algorithms` or `deep-rl` branches might be:
- #10 (easy) - Convert algorithms into agents
- #11 (med) - Add a testable "convergence" criteria
- #12 (med) - Implement some eligibility trace variants to the `algorithms` branch
- #13 (med) - Add some policy gradient methods to the `algorithms` branch
- #14 (hard) - Head over to the `deep-rl` branch and convert some of the deep reinforcement learning models into haskell with [tensorflow-haskell][tfhs], and/or [backprop][bp]

For a longer-term view, feel free to check out [MILESTONES.md](./MILESTONES.md).

[tfhs]:https://github.com/tensorflow/haskell
[bp]:https://github.com/mstksg/backprop

Contributors
======================

Thanks goes to these wonderful people ([emoji key](https://github.com/kentcdodds/all-contributors#emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
| [<img src="https://avatars3.githubusercontent.com/u/1694705?v=4" width="60px;"/><br /><sub>Sam Stites</sub>](https://www.stites.io)<br />[💻](https://github.com/stites/reinforce/commits?author=stites "Code") [🤔](#ideas-stites "Ideas, Planning, & Feedback") [📖](https://github.com/stites/reinforce/commits?author=stites "Documentation") | [<img src="https://avatars2.githubusercontent.com/u/1074598?v=4" width="60px;"/><br /><sub>Mitchell Rosen</sub>](https://github.com/mitchellwrosen)<br />[🤔](#ideas-mitchellwrosen "Ideas, Planning, & Feedback") | [<img src="https://avatars0.githubusercontent.com/u/1494102?v=4" width="60px;"/><br /><sub>Anastasia Aizman</sub>](https://github.com/anastasia)<br />[📖](https://github.com/stites/reinforce/commits?author=anastasia "Documentation") |
| :---: | :---: | :---: |
<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/kentcdodds/all-contributors) specification. Contributions of any kind welcome!

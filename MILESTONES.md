Milestones
===================

Currently, MonadEnv works as needed to create baseline agents.
Some rough Milestones for `reinforce-*` would be the following:

### v0.1.0

+ (envs) Formalize ActionSpace and StateSpace
+ (envs) Port over all gym-atari environments
+ (envs) Add universe environment
+ (envs) Add roboschool environment
+ (envs) Figure out how to speed up gym-http-api code (call to python directly?)

### v0.2.0

+ (envs) Possibly split `reinforce` into third-party dependencies (ie: `-envs-gym` and `-envs-atari`)
+ (envs) Introduce some kind of `History` monad to cleanly seperate debug logging from agent or environment reporting
+ (algos) write out more baseline algorithms in reinforce-algorithms (in progress)
+ (algos) modify project structure to so that reinforce and reinforce-algorithms are siblings

### v0.3.0

+ (envs) Have some kind of reporting system (live reporting?) or server in place for model inspection
+ (algos) port all v0.2.0 python prototypes into backprop in reinforce-deep-algorithms
+ (algos) port all v0.2.0 python prototypes into tensorflow-haskell in reinforce-deep-algorithms


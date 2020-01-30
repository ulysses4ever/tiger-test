# tiger-test

This is a small and dirty autograder (it's meant to be big and robust).

## Dependencies

It's a Haskell application -- so you have to have basic Haskell tools in order to builld it. First of all, (fairly recent versions of) the _GHC compiler_ and the _Cabal_ build tool. Say, GHC 8.6 and Cabal 2.4. If you're unsure of how to get them with your system's package manager, you could use [ghcup](https://www.haskell.org/ghcup/) (preffered on a *nix platform) or [Haskell Platform](https://www.haskell.org/platform/) (preffered on Windows), or download binaries from official websites. On Ubuntu, [hvr's ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc) is handy for that.

## Overview

To start using the grader, you most likely will have to adjust parameters at the top of `Constants.hs` (everything of form `*.hs` mentioned here refers to constents ofthe `src` directory). It has some hard-coded paths for the stuff you're going to grade.

"Small" tests live in `Constants.hs` at the moment too. More complicated tests might require more heavy-weight machinery of `CompareOutputs.hs`.

Besides `Constant.hs`, important bits are:

* `Main.hs` -- entry point, a small and not too important;

* `Core.hs` -- mainly, just looping over directories (`enumerateSubmissions`). A sudmission is a dir containing 

* `RecordOutput.hs` -- the main machinery to run submissions and save their outputs. Optionally, test against "small" tests from `Constants.hs`.


## Building & Running

`make` & `make run` should do the thing provided the dependencies are satisfied. No arguments are needed: everything is set up via constants inside `Constants.hs`.


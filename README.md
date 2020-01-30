# tiger-test

This is a small and dirty autograder (it's meant to be big and robust).

## Dependencies

It's a Haskell application -- so you have to have basic Haskell tools in order to builld it. First of all, (fairly recent versions of) the _GHC compiler_ and the _Cabal_ build tool. Say, GHC 8.6 and Cabal 2.4.

## Overview

To start using the grader, you most likely will have to adjust parameters at the top of `Constants.hs`. It has some hard-coded paths for the stuff you're going to grade.

"Small" tests live in `Constants.hs` at the moment too. More complicated tests might require more heavy-weight machinery of `CompareOutputs.hs`.

Besides `Constant.hs`, important bits are:

* `Main.hs` -- entry point, a small and not too important;

* `Core.hs` -- mainly, just looping over directories (`enumerateSubmissions`). A sudmission is a dir containing 

* `RecordOutput.hs` -- the main machinery to run submissions and save their outputs. Optionally, test against "small" tests from `Constants.hs`.


## Building & Running

`make` & `make run` should do the thing provided the dependencies are satisfied.

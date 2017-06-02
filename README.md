# Profiterole [![Hackage version](https://img.shields.io/hackage/v/profiterole.svg?label=Hackage)](https://hackage.haskell.org/package/profiterole) [![Stackage version](https://www.stackage.org/package/profiterole/badge/lts?label=Stackage)](https://www.stackage.org/package/profiterole) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/profiterole.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/profiterole) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/profiterole.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/profiterole)

Script for reading and restructring a GHC profile script.

## The Goal

Given profile data, different ways of looking at it reveal different insights. This tool provides one of those insights - in addition to reading the standard profile output and using other tools such as [Profiteur](https://hackage.haskell.org/package/profiteur).

Profiterole aims to make the profile shorter by combining common subtrees and lifting them to the root - e.g. if you call `parseFile` from 7 places in the code, instead of having 7 pieces of `parseFile` profiling, Profiterole will give you one.

## Usage

To run, first install (`cabal update && cabal install profiterole`), generate a GHC profile the [normal way](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html), then run:

    profiterole myprogram.prof

Profiterole will generate `myprogram.profiterole.txt` and `myprogram.profiterole.html` - both contain the same information, but the HTML has hyperlinks.

For large programs, using `+RTS -P` (instead of the common `-p`) will give more accurate results.

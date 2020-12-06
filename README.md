# HaskellTemporalLib
Utilities for Working with Temporal Networks

This library provides types and functions to solve Temporal Problems using
Temporal Networks.

Temporal Networks are a way to represent time constraints between events, and
are useful in solving autonomous planning problems with continuous time.

## Installing From GitHub Source / Local Package Cabal Install
Using Cabal:

```bash
git clone 'https://github.com/CrystalSplitter/HaskellTemporalLib.git' "$BUILD_LOC"
cd "$BUILD_LOC"

# In $BUILD_LOC
cabal update
cabal build
cabal install --lib

# You can now delete the $BUILD_LOC if you so desire.
```

## Uninstalling From Cabal
Usually the package files will be located in `$HOME/.cabal/store/ghc-8.8.4/`,
unless you have a custom install set up for `cabal`.

You may need to clear your default loaded packages as well in
`$HOME/.ghc/x86_64-linux-8.8.4/environments/default`.

## Running Tests
```bash
# In $BUILD_LOC
cabal test
```

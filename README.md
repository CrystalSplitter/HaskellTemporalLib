# HaskellTemporalLib
*Utilities for working with Temporal Networks.*

[![License LGPLv3](https://img.shields.io/github/license/CrystalSplitter/HaskellTemporalLib?style=for-the-badge)](https://opensource.org/licenses/LGPL-3.0)

---

This library provides types and functions to solve Temporal Problems using
Temporal Networks.

Temporal Networks are a way to represent time constraints between events, and
are useful in solving autonomous planning problems with continuous time.

## Installing From GitHub Source / Local Package Cabal Install

### External Dependencies:
  * gcc
  * ghc (easily installed with ghcup)
  * cabal (easily installed with ghcup)
  * [libgmp-dev](https://packages.debian.org/buster/libgmp-dev)

### Installation:

```bash
git clone 'https://github.com/CrystalSplitter/HaskellTemporalLib.git' "$BUILD_LOC"
cd "$BUILD_LOC"

# In $BUILD_LOC
cabal v2-update
cabal v2-build
cabal v2-install --lib

# You can now delete the $BUILD_LOC if you so desire.
```

## Generating Haddock

```bash
# In $BUILD_LOC
cabal v2-haddock
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

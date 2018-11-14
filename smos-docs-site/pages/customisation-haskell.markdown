---
title: Customisation in Haskell
date: 2018-11-13
---


### Disclaimer

Smos is available via a default build without any programming involved.
If you are looking to do that, please see [the respective guide](/customisation-default.html)

## Your own smos

Smos is not a program as much as it is a library to write your own version of smos.
(This is a lot like the way [Xmonad](/TODO) does things.)


### Set up a Haskell project

To customise Smos within Haskell for yourself, you will need to set up your own little Haskell project.
You can do this with either [stack](/TODO) or with Nix.
In this tutorial we will use stack, because I assume that if you want to use Nix, you know what you are doing and you will be able to figure it out.

To get started, have `stack` setup a new project:

```
stack new my-smos
cd my-smos
```

#### Getting the smos dependency

Next, you need to make sure that `stack` knows where to find `smos`.
By the time you read this, the relevant packages may already be on stackage so this step may not be necessary, but to make sure that this guide gives reproducible results, we add get the sources directly:
To make that happen, add the following to your `stack.yaml`:

```
extra-deps:
- git: https://github.com/NorfairKing/smos.git
  # Use a recent commit from the 'master' branch
  commit: 598e3e917b22c3ab1f3bffc3645424e4a7a568c7
  subdirs:
    - smos
    - smos-archive
    - smos-convert-org
    - smos-cursor
    - smos-cursor-gen
    - smos-data
    - smos-data-gen
    - smos-query
    - smos-report
    - smos-report-cursor
    - smos-report-cursor-gen
    - smos-report-gen
- git: https://github.com/NorfairKing/pretty-relative-time.git
  commit: 0044ce34d484d54e6809c30cee8e1219c722765f
- git: https://github.com/NorfairKing/cursor-fuzzy-time.git
  commit: 8b1358f768b709651efb90f8f7bd2d84f70cd0ae
  subdirs:
    - cursor-fuzzy-time
    - cursor-fuzzy-time-gen
- git: https://github.com/NorfairKing/fuzzy-time.git
  commit: 79cac898f475036f08022992ebc764026ce510e5
  subdirs:
    - fuzzy-time
    - fuzzy-time-gen
- git: https://github.com/NorfairKing/cursor.git
  commit: a1c35161d308289b4b0af2aa228a6898fa367d36
  subdirs:
    - cursor
    - cursor-gen
- git: https://github.com/NorfairKing/validity.git
  commit: 70874acf594ec3667b974032379aa29c961e5866
  subdirs:
  - genvalidity
  - genvalidity-aeson
  - genvalidity-bytestring
  - genvalidity-containers
  - genvalidity-hspec
  - genvalidity-hspec-aeson
  - genvalidity-hspec-binary
  - genvalidity-hspec-cereal
  - genvalidity-hspec-hashable
  - genvalidity-hspec-optics
  - genvalidity-path
  - genvalidity-property
  - genvalidity-scientific
  - genvalidity-text
  - genvalidity-time
  - genvalidity-unordered-containers
  - genvalidity-uuid
  - genvalidity-vector
  - validity
  - validity-aeson
  - validity-bytestring
  - validity-containers
  - validity-path
  - validity-primitive
  - validity-scientific
  - validity-text
  - validity-time
  - validity-unordered-containers
  - validity-uuid
  - validity-vector
```

Add `smos` to the project dependencies in `package.yaml`:

```
library:
  source-dirs: src
  dependencies:
  - smos
```

Compile the project with this extra dependency:

```
stack build
```

#### Remaking the default smos

In `src/Lib.hs`, replace the contents by this:

``` haskell
module Lib
    ( someFunc
    ) where

import Smos
import Smos.Default

someFunc :: IO ()
someFunc = smos defaultConfig
```

Compile this with `stack build`.
Now you should be able to run `stack exec my-smos-exe`.
Now you can start customising the configuration that you pass to the `smos` function to your heart's content.

Note: The `smos` function will also deal with flags, environment variables and configuration files. If you want to make your customisations without these features, you can use [`smosWithoutRuntimeConfig`](/TODO) instead.

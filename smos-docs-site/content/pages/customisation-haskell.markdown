---
title: Customisation in Haskell
date: 2018-11-13
---


### Disclaimer

Smos is available via a default build without any programming involved.
If you are looking to do that, please see [the respective guide](/customisation-default.html)

## Your own smos

Smos is not a program as much as it is a library to write your own version of smos.
(This is a lot like the way [Xmonad](https://xmonad.org/) does things.)


### Set up a Haskell project

To customise Smos within Haskell for yourself, you will need to set up your own little Haskell project.
You can do this with either [stack](https://haskellstack.org) or with Nix.
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
  commit: 4e7eb8c83851847546c15e4b33a4bb9984a4a55b
  subdirs:
    - smos
    - smos-data
    - smos-data-gen
    - smos-cursor
    - smos-cursor-gen
    - smos-report
    - smos-report-gen
    - smos-report-cursor
    - smos-report-cursor-gen
    - smos-query
    - smos-convert-org
    - smos-archive
    - smos-docs-site
- git: https://github.com/NorfairKing/pretty-relative-time.git
  commit: 21b29c9d729ed91a56819f569de6fdc8582d7e3d
- git: https://github.com/NorfairKing/cursor-fuzzy-time.git
  commit: 251f53f7bf12997f2f14ff445fdff818f41f03e8
  subdirs:
    - cursor-fuzzy-time
    - cursor-fuzzy-time-gen
- git: https://github.com/NorfairKing/fuzzy-time.git
  commit: e496ad8924cebea70dd81f85325e92a6a658475d
  subdirs:
    - fuzzy-time
    - fuzzy-time-gen
- git: https://github.com/NorfairKing/cursor-brick.git
  commit: 178d50eac204cdae3d3aeb92c673b18f6245a4cb
  subdirs:
    - cursor-brick
- git: https://github.com/NorfairKing/cursor.git
  commit: 0cedf6a4685648d47e82285e2250dd9a6177307d
  subdirs:
    - cursor
    - cursor-gen
- git: https://github.com/NorfairKing/mergeful
  commit: b14dde7a01cb12d7a2eec180c03de135059e36c6
  subdirs:
    - mergeful
    - genvalidity-mergeful
- git: https://github.com/NorfairKing/validity.git
  commit: 8a13a23696115845133d87118b81c47fa1f9b04b
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

Note: The `smos` function will also deal with flags, environment variables and configuration files. If you want to make your customisations without these features, you can use `smosWithoutRuntimeConfig` instead.

---
title: Customisation in Haskell
---


### Disclaimer

Smos is available via a default build without any programming involved.
If you are looking to customise smos without writing any code, please see [the respective guide](/customisation-default.html)

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

To make that happen, copy the "extra-deps" section of [Smos' `stack.yaml`](https://github.com/NorfairKing/smos/blob/master/stack.yaml) to your `stack.yaml`.

```
extra-deps:
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
If you are unsure where to start, you can have a look at [the default configuration](https://github.com/NorfairKing/smos/blob/master/smos/src/Smos/Default.hs).

Note: The `smos` function will also deal with flags, environment variables and configuration files. If you want to make your customisations without these features, you can use `smosWithoutRuntimeConfig` instead.

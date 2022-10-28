---
title: Contributing
description: An contribution guide for developers that are new to the smos project
---


1. Clone the repository:

   ```
   $ git clone git@github.com:NorfairKing/smos.git --recursive
   ```

   If you've already cloned the repository but forgot about the `--recursive` flag, you can run this:

   ```
   $ git submodule update --init --recursive
   ```

1. We use Nix for CI and deployment.
   Make sure you can run `nix develop`.
   This also ensures that the pre-commit hooks are installed.
1. We use stack for development.
   Make sure you can run `stack test smos` after following the steps in [the stack installation guide](/installation/stack). 
1. Checkout the `development` branch:

   ```
   $ git checkout development
   ```

1. Enter the nix shell

   ```
   $ nix develop
   ```

1. Set up your feedback loop.
   When developing in a single smos package, you can use

   ```
   nix $ stack test <thepackage> --pedantic --file-watch
   ```

   When developing accross multiple smos packages, you can use

   ```
   nix $ stack test --pedantic --file-watch --no-rerun-tests
   ```

   Some feedback loops are provided, as you should have seen when you ran `nix develop`.
   You may want to check if those fit your needs first.

1. Optionally: Set up a local hoogle server to look up code documentation for smos or its dependencies:

   ```
   nix $ hoogle serve --local
   ```

1. Make your changes.

1. Make sure the following commands succeeds locally:

   ```
   nix $ stack clean
   nix $ stack build --test --bench --no-run-benchmarks --pedantic`
   ```

1. Make sure `nix flake check` succeeds locally.

1. Create a pull request with `NorfairKing/development` as the base.

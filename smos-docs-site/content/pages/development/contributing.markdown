---
title: Contributing
description: An contribution guide for developers that are new to the smos project
---


1. Clone the repository:

   ```
   git clone git@github.com:NorfairKing/smos.git --recursive
   ```

   If you've already cloned the repository but forgot about the `--recursive` flag, you can run this:

   ```
   git submodule update --init --recursive
   ```

1. We use nix for CI and deployment. Make sure you can run `nix-shell` as described in [the nix installation guide](/installation/nix).
   This also ensures that the pre-commit hoosk are installed.
1. We use stack for development. Make sure you can run `stack test smos` after following the steps in [the stack installation guide](/installation/stack). 
1. Checkout the `development` branch:

   ```
   git checkout development
   ```

1. Set up your feedback loop.

   When developing in a single smos package, you can use

   ```
   stack test <thepackage> --pedantic --file-watch
   ```

   When developing accross multiple smos packages, you can use

   ```
   stack test --pedantic --file-watch --no-rerun-tests
   ```
  
1. Make your changes.
1. Make sure `stack build --test --bench --no-run-benchmarks --pedantic` succeeds locally.
1. Make sure `nix-build ci.nix` succeeds locally.
1. Create a pull request with `NorfairKing/development` as the base.

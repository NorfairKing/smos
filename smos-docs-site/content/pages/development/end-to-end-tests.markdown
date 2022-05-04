---
title: End to end testing
description: A description of how we do end to end testing
---

We run these end-to-end tests:

* `candidate` -> `candidate`: Current compatibility of the release candidate
* `candidate` -> `release`: Backward compatibility
* `release` -> `candidate`: Forward compatibility

The Haskell code for these tests can be found in `smos-server-gen/app`.
The NixOS module for running end-to-end tests can be found in `nix/end-to-end-test-nixos-module.nix`.
The NixOS test that actually does the running of these tests is in `nix/e2e-test.nix`.
You can run each one from `ci.nix`.

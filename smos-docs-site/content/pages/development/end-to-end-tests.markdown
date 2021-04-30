---
title: End to end testing
description: A description of how we do end to end testing
---

We run these end-to-end tests:

* `testing` -> `testing`: Current compatibility of the release candidate
* `testing` -> `staging`: Backward compatibility
* `staging` -> `testing`: Forward compatibility
* `staging` -> `staging`: Current compatibility of the previous release

The code for these tests can be found in `smos-server-gen/app`.
The nixos module for running thim can be found in `nix/end-to-end-test-nixos-module.nix`.
We also run the end-to-end tests against the current version in `ci.nix` via the nixos test in `nix/nixos-end-to-end-test-test.nix`.

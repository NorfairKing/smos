---
title: Release checklist
description: A checklist of what to do when we are ready to make a new release
---

When a given commit on the `development` branch is supposedly ready for release, we go through the following checklist:

1. Make sure that the version numbers of each package that's been changed has been updated, using `git diff release`.
1. Make sure that the version number of the data format has been changed if the data format has been changed.
1. Make sure that the version number of the API has been changed if the API has been changed.
1. Add new release section in the changelog.
1. Run `stack runhaskell scripts/gen-changelog-release-section.hs` to add all the version numbers to the changelog.
1. Make sure that CI passes, remotely as well as with `ci.nix`.
1. Make a release candidate commit.
1. Merge `development` into `release`.
1. Run the release script to create the appropriate tags: `stack runhaskell scripts/make-release-tags.hs`.
1. Push to github with `git push`.
1. Push the tags with `git push --tags`
1. Make a GitHub release with the contents of the changelog using `nix-shell --run ./scripts/make-github-release.sh`.
1. Deploy to production.
1. Double-check that `development` and `release` now all point to the same version.

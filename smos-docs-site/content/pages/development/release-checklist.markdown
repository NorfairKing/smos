---
title: Release checklist
description: A checklist of what to do when we are ready to make a new release
---

When a given commit on the `development` branch is supposedly ready for release, we go through the following checklist:

1. Update the version numbers of each package that's been changed using `git diff release`.
1. Update the version number of the data format if the data format has been changed.
1. Update the version number of the API if the API has been changed.
1. Add new release section in the changelog.
1. Run `stack runhaskell scripts/gen-changelog-release-section.hs` to add all the version numbers to the changelog.
1. Make sure that CI passes.
1. Make a release candidate commit.
1. Merge `development` into `testing`.
1. Deploy to `testing` and wait for the end-to-end tests to pass.
   If the end-to-end tests don't pass, that means we broke either forward or backward compatibility.
   In that case we revert testing to the previous release commit and start over with the release checklist.
1. Merge `testing` into `staging`.
1. Deploy to staging.
1. Wait for end-to-end tests to pass again.
1. Merge `staging` into `release`.
1. Run the release script to create the appropriate tags: `stack runhaskell scripts/make-release-tags.hs`.
1. Make a GitHub release with the contents of the changelog.
1. Double-check that `development`, `testing`, `staging` and `release` now all point to the same version.

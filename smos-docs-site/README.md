# Smos Docs Site

## Release checklist

1. Make sure that CI passes on development.
2. Merge development into `master`.
3. Update version numbers.
4. Add new release section in the changelog.
5. Update the release branch to the latest commit.
6. Run the release script.
7. Make a GitHub release with the contents of the changelog.
8. Upload the release-zip with static binaries to the artefacts of the GitHub release
9. Update master and development to the latest commit, to continue on `development`.

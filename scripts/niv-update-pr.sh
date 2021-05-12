#! /usr/bin/env nix-shell
#! nix-shell ./niv-update-pr.nix -i bash --keep GITHUB_TOKEN

niv update autorecorder
niv update bulma
niv update bulma-carousel
niv update gitignore.nix
niv update niv
niv update nix-pre-commit-hooks
niv update nixpkgs
git submodule update --init --recursive

if ! git diff-index --quiet HEAD
then
  today="$(date -I)"
  branchname="package-update-$today"
  git checkout -b "$branchname"
  git config user.name "Niv updater"
  pre-commit run
  git add .
  git commit -m "Automatic niv-update $today"
  git push -u origin "$branchname"
  gh auth login --with-token <<< "$GITHUB_TOKEN"
  gh pr create \
    --base development \
    --head "$branchname" \
    --title "Automatic niv-update - $today" \
    --body ""
  gh pr merge "$branchname" --auto --squash
fi

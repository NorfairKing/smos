set -ex

case $BUILD_KIND in
  stack)
    stack --no-terminal $RESOLVER_FLAG build --haddock --pedantic
    stack --no-terminal $RESOLVER_FLAG test --pedantic
    ;;
  nix)
    nix-build -A haskellPackages.cursor
    nix-build -A haskellPackages.cursor-gen
    nix-build -A haskellPackages.smos-data
    nix-build -A haskellPackages.smos-data-gen
    ;;
  *)
    echo "Unknown build kind."
    ;;
esac

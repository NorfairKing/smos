set -ex

case $BUILD_KIND in
  stack)
    stack --no-terminal $RESOLVER_FLAG build --haddock --pedantic
    stack --no-terminal $RESOLVER_FLAG test --pedantic
    ;;
  nix)
    nix-build nix/release.nix --no-out-link
    ;;
  *)
    echo "Unknown build kind."
    ;;
esac

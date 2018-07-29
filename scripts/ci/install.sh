set -ex

case $BUILD_KIND in
  stack)
    stack setup $RESOLVER_FLAG
    stack build --only-snapshot $RESOLVER_FLAG
    ;;
  nix)
    echo "Nothing to do for install in a nix build."
    ;;
  *)
    echo "Unknown build kind."
    ;;
esac

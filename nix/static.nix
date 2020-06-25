# Run using:
#
#     $(nix-build nix/static.nix --no-link -A releaseTarScript)
{ stack2nix-output-path ? "custom-stack2nix-output.nix"
,
}:
let
  cabalPackageNames = [
    "smos"
    "smos-query"
    "smos-archive"
    "smos-single"
    "smos-convert-org"
    "smos-sync-client"
    # # "smos-scheduler" # Not production ready
    # # "smos-calendar-import" # Not production ready
    # # "smos-server" # Not for the casual user
    # # "smos-web-server" # Not for the casual user
  ];
  compiler = "ghc865"; # matching stack.yaml

  # Pin static-haskell-nix version.
  static-haskell-nix = fetchTarball https://github.com/nh2/static-haskell-nix/archive/d1b20f35ec7d3761e59bd323bbe0cca23b3dfc82.tar.gz;

  # Pin nixpkgs version
  # By default to the one `static-haskell-nix` provides, but you may also give
  # your own as long as it has the necessary patches, using e.g.
  #     pkgs = import (fetchTarball https://github.com/nh2/nixpkgs/archive/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa123.tar.gz) {};
  pkgs = import (fetchTarball https://github.com/nh2/nixpkgs/archive/0c960262d159d3a884dadc3d4e4b131557dad116.tar.gz) { config.allowUnfree = true; };


  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ../.; # where stack.yaml is
    hackageSnapshot = "2020-06-25T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = name:
    import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
      normalPkgs = pkgs;
      inherit compiler stack2nix-output-path;
      cabalPackageName = name;
      # disableOptimization = true; # for compile speed
    };

  staticPackageFlag = name: "-A static_package.${name}";
  staticPackageFlags = pkgs.lib.concatMapStringsSep " " staticPackageFlag cabalPackageNames;

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript =
    pkgs.writeShellScript "stack2nix-and-build-script.sh" ''
      set -eu -o pipefail
      STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
      export NIX_PATH=nixpkgs=${pkgs.path}

      ${pkgs.nix}/bin/nix-build nix/static.nix ${staticPackageFlags} --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
    '';
  static_package = pkgs.lib.genAttrs cabalPackageNames (
    name:
      pkgs.haskell.lib.addBuildDepend ((static-stack2nix-builder name).static_package) (pkgs.haskellPackages.autoexporter)
  );
  releaseTarScript =
    pkgs.writeShellScript "smos-release-zip.sh" ''
      for i in *result
      do
        unlink $i
      done
      ${fullBuildScript}

      find result*/bin -type f | xargs ${pkgs.gnutar}/bin/tar -cf smos.tar.gz
    '';
in
{
  inherit static_package;
  inherit fullBuildScript;
  inherit releaseTarScript;
  # For debugging:
  inherit stack2nix-script;
  inherit static-stack2nix-builder;
}

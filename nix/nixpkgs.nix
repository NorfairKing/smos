let nixpkgsVersion = import ./nixpkgs-version.nix;
in builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
  inherit (nixpkgsVersion) sha256;
}

let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.smosRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "nixos-module-test" = import ./nix/nixos-module-test.nix {
    inherit sources;
    inherit pkgs;
  };
  "nixos-end-to-end-test" = import ./nix/nixos-end-to-end-test-test.nix {
    inherit sources;
    inherit pkgs;
  };
}

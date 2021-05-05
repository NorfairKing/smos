let
  pkgsNormal = import ./nix/pkgs.nix { static = false; };
  pkgsStatic = import ./nix/pkgs.nix { static = true; };
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
{
  "release" = pkgsNormal.smosRelease;
  "release-static" = pkgsStatic.smosRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "release-zip" = pkgsStatic.smosReleaseZip;
  "nixos-module-test" = import ./nix/nixos-module-test.nix { pkgs = pkgsNormal; };
  "nixos-end-to-end-test" = import ./nix/nixos-end-to-end-test-test.nix { pkgs = pkgsNormal; };
}

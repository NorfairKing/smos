let
  pkgs = import ./nix/pkgs.nix { static = false; };
  pkgsNormal = import ./nix/project.nix { inherit pkgs; };
  pkgsStatic = import ./nix/project.nix { pkgs = import ./nix/pkgs.nix { static = true; }; };
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
{
  "release" = pkgsNormal.smosRelease;
  "casts" = pkgsNormal.smosCasts;
  "release-static" = pkgsStatic.smosRelease;
  "casts-static" = pkgsStatic.smosCasts;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "release-zip" = pkgsStatic.smosReleaseZip;
  "nixos-module-test" = import ./nix/nixos-module-test.nix { inherit pkgs; };
}

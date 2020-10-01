let
  pkgsNormal = import ./nix/project.nix { pkgs = import ./nix/pkgs.nix { static = false; }; };
  pkgsStatic = import ./nix/project.nix { pkgs = import ./nix/pkgs.nix { static = true; }; };
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
{
  "release" = pkgsNormal.smosRelease;
  "release-static" = pkgsStatic.smosRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
}

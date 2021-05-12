let
  sources = import ./nix/sources.nix;
  pkgsNormal = import ./nix/pkgs.nix { inherit sources; static = false; };
  pkgsStatic = import ./nix/pkgs.nix { inherit sources; static = true; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgsNormal.smosRelease;
  "release-static" = pkgsStatic.smosRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "release-zip" = pkgsStatic.smosReleaseZip;
  "nixos-module-test" = import ./nix/nixos-module-test.nix {
    inherit sources;
    pkgs = pkgsNormal;
  };
  "nixos-module-test-static" = import ./nix/nixos-module-test.nix {
    inherit sources;
    pkgs = pkgsNormal;
    smosPackages = pkgsStatic.smosPackages;
  };
  "nixos-end-to-end-test" = import ./nix/nixos-end-to-end-test-test.nix {
    inherit sources;
    pkgs = pkgsNormal;
  };
  "nixos-end-to-end-test-static" = import ./nix/nixos-end-to-end-test-test.nix {
    inherit sources;
    pkgs = pkgsNormal;
    smosPackages = pkgsStatic.smosPackages;
  };
}

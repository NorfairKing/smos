let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.smosRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "nixos-module-test-current-compatibility" = import ./nix/e2e-test.nix {
    pathUnderTest = ./.;
    sourcesUnderTest = sources;
    pkgsUnderTest = pkgs;
    pathOverTest = ./.;
    sourcesOverTest = sources;
    pkgsOverTest = pkgs;
  };
  "nixos-module-test-backward-compatibility" = import ./nix/e2e-test.nix {
    pathUnderTest = ./.;
    sourcesUnderTest = sources;
    pkgsUnderTest = pkgs;
    pathOverTest = sources.smos-latest-release;
  };
  "nixos-module-test-forward-compatibility" = import ./nix/e2e-test.nix {
    pathUnderTest = sources.smos-latest-release;
    pathOverTest = ./.;
    sourcesOverTest = sources;
    pkgsOverTest = pkgs;
  };
}

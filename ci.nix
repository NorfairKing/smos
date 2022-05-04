let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.smosRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "e2e-test-current-compatibility" = import ./nix/e2e-test.nix {
    name = "current-compatibility";
    pathUnderTest = ./.;
    sourcesUnderTest = sources;
    pkgsUnderTest = pkgs;
    pathOverTest = ./.;
    sourcesOverTest = sources;
    pkgsOverTest = pkgs;
  };
  "e2e-test-backward-compatibility" = import ./nix/e2e-test.nix {
    name = "backward-compatibility";
    pathUnderTest = ./.;
    sourcesUnderTest = sources;
    pkgsUnderTest = pkgs;
    pathOverTest = sources.smos-latest-release;
  };
  "e2e-test-forward-compatibility" = import ./nix/e2e-test.nix {
    name = "forward-compatibility";
    pathUnderTest = sources.smos-latest-release;
    pathOverTest = ./.;
    sourcesOverTest = sources;
    pkgsOverTest = pkgs;
  };
}

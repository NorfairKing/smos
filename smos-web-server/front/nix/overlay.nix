final: previous:


let
  easyPS = import (
    final.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "2432d492b1f0cc3aa7bf8b4bdb2c7ee0219e1f4e";
      sha256 = "sha256:0gdg7b4yvs9rg2z6xv705miq5c9vbhimq21nfmhk9j48b3m3xkhf";
    }
  ) { pkgs = final; };
in
  with easyPS;
  {
    inherit (easyPS)
      purs
      purty
      spago
      ;
    spago2nix = import (
      final.fetchFromGitHub {
        owner = "justinwoo";
        repo = "spago2nix";
        rev = "96d0fd2ab96e62ad5fa2d5f0dd086652a2ac2901";
        sha256 = "sha256:1kzv2y970x0vii2pgfhpyg9q77vw3zz9p370z70sqkc6m424j2vb";
      }
    ) {
      pkgs = final;
    };


    smos-web-server-front =
      let
        spagoPkgs = import ../spago-packages.nix { pkgs = final; };
        projectOutput = spagoPkgs.mkBuildProjectOutput {
          src = final.gitignoreSource ../.;
          purs = final.purs;
        };
      in
        final.stdenv.mkDerivation rec {
          name = "smos-web-server-front";
          src = final.gitignoreSource ../.;
          buildInputs = with final; [
            purs
          ];
          installPhase = with final; ''
            purs bundle ${projectOutput}/output/**/*.js --module Main --main Main --output $out/dist/smos-web-server-front.js
          '';
        };
  }

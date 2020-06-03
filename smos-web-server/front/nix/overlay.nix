final: previous:


let
  easyPS = import (
    final.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0ba91d9aa9f7421f6bfe4895677159a8a999bf20";
      sha256 = "1baq7mmd3vjas87f0gzlq83n2l1h3dlqajjqr7fgaazpa9xgzs7q";
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

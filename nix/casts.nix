pkgs:
with pkgs;
let
  specFiles =
    builtins.map (lib.removeSuffix ".yaml")
      (
        builtins.attrNames
          (
            lib.filterAttrs
              (p: v: v == "regular" && lib.hasSuffix ".yaml" p) # TODO: filter by extension too?
              (builtins.readDir ../smos-asciinema/examples)
          )
      );
  derivationFor = path:
    stdenv.mkDerivation {
      name = "smos-asciinema-casts-${path}";
      buildInputs = [
        asciinema
        ncurses
        tree
      ] ++ lib.attrValues smosPackages;
      src = gitignoreSource ../.;
      ASCIINEMA_CONFIG_HOME = "./config";
      buildCommand = ''
        # Make sure the spec files and the demo-workflow are available
        cp -r $src/demo-workflow ./demo-workflow
        mkdir -p ./smos-asciinema
        cp -r $src/smos-asciinema/examples ./smos-asciinema/examples

        # Make sure we're using a terminal we know about.
        export TERM=xterm-256color

        # Make sure they are writeable too
        chmod -R +w .

        # Make sure asciinema has place to write its config to
        mkdir -p $ASCIINEMA_CONFIG_HOME

        # Record the cast
        smos-asciinema record "./smos-asciinema/examples/${path}.yaml" "./${path}.cast" \
          --columns 80 \
          --rows 25

        # Output the casts
        cp "${path}.cast" $out
      '';
    };
in
builtins.map derivationFor specFiles

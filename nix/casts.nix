{ lib
, stdenv
, smosPackages
, asciinema
, gitignoreSource
, ncurses
, rxvt_unicode
, python
}:
stdenv.mkDerivation {
  name = "smos-asciinema-casts";
  buildInputs = [ asciinema ncurses python ] ++ lib.attrValues smosPackages;
  src = gitignoreSource ../.;
  ASCIINEMA_CONFIG_HOME = "./config";
  buildCommand = ''
    # Set terminal size
    python ${../smos-asciinema/set_window_size.py} 80 25

    # Make sure the spec files and the demo-workflow are available
    cp -r $src/demo-workflow ./demo-workflow
    mkdir -p ./smos-asciinema
    cp -r $src/smos-asciinema/examples ./smos-asciinema/examples

    # Make sure they are writeable too
    chmod -R +w .

    # Make sure asciinema has place to write its config to
    mkdir -p $ASCIINEMA_CONFIG_HOME

    mkdir -p $out/casts
    for i in ./smos-asciinema/examples/*
    do
      local base="$(basename $i .yaml)"
      # Record the cast
      smos-asciinema record "./smos-asciinema/examples/$base.yaml" "./$base.cast"

      # Output the casts
      cp "$base.cast" $out/casts
    done
  '';
}

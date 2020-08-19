pkgs:
with pkgs;
let
  castsDir = ../smos-docs-site/content/casts;
  specFiles =
    builtins.map (lib.removeSuffix ".yaml")
      (
        builtins.attrNames
          (
            lib.filterAttrs
              (p: v: v == "regular" && lib.hasSuffix ".yaml" p) # TODO: filter by extension too?
              (builtins.readDir castsDir)
          )
      );
  derivationFor = path:
    stdenv.mkDerivation {
      name = "smos-asciinema-casts-${path}";
      buildInputs = with pkgs.smosPackages; [
        ncurses
        tree
        smos
        smos-query
        smos-archive
        smos-asciinema
        smos-calendar-import
      ]; # Add the smos packages here one by one because smos-docs-site shouldn't be in here.
      buildCommand =
        let
          # This needs to be run on shell startup for backspace and enter to work
          # correctly but it cannot be run from a script beforehand because it
          # only works in (pseudo) terminals.
          bashRC = writeText "bashrc" ''
            stty sane

            export PS1="\\$ "
          '';
        in
          ''
            # Make sure the spec files and the demo-workflow are available
            mkdir -p ./smos-docs-site
            cp -r ${gitignoreSource ../smos-docs-site/demo-workflow} ./smos-docs-site/demo-workflow
            mkdir -p ./smos-docs-site/content/casts
            cp -r ${castsDir + "/${path}.yaml"} ./smos-docs-site/content/casts/${path}.yaml

            # Make sure they are writeable too
            chmod -R +w .

            # To make sure that the right colours are used.
            export TERM=xterm-256color

            # To make sure that backspace works, see above
            export SHELL="${bash}/bin/bash --rcfile ${bashRC}"

            # To make sure that programs like 'tree' show nice unicode characters
            export LANG=C.utf8
            export LC_ALL=C.utf8

            # To show what's happening
            export SMOS_EXPLAINER_MODE=True

            # Record the cast
            smos-asciinema record "./smos-docs-site/content/casts/${path}.yaml" "./${path}.cast" \
              --columns 80 \
              --rows 25 \
              --progress

            # Output the cast
            cp "${path}.cast" $out
          '';
    };
in
pkgs.lib.genAttrs specFiles derivationFor

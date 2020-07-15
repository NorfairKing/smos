{ lib, stdenv, smosPackages, asciinema, gitignoreSource, ncurses }:
stdenv.mkDerivation {
  name = "smos-asciinema-casts";
  buildInputs = [ asciinema ncurses ] ++ lib.attrValues smosPackages;
  src = gitignoreSource ../.;
  buildCommand = ''

    mkdir -p $out
    mkdir -p $out/config

    cp -r $src/demo-workflow $out/demo-workflow
    mkdir -p $out/smos-asciinema
    cp -r $src/smos-asciinema/examples $out/smos-asciinema/examples

    chmod -R +w $out

    shopt -u checkwinsize
    export LINES=80
    export COLUMNS=80
    export TERM=xterm
    tput lines
    tput cols

    export ASCIINEMA_CONFIG_HOME=$out/config
    smos-asciinema record $out/smos-asciinema/examples/waiting.yaml $out/waiting.cast
  '';
}

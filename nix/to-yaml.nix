{ stdenv, writeText, haskellPackages }:
name:
config:
stdenv.mkDerivation {
  inherit name;
  buildInputs = [ haskellPackages.json2yaml ];
  src = "${writeText "${name}.yaml" (builtins.toJSON config)}";
  buildCommand = ''
    json2yaml $src > $out
  '';
}

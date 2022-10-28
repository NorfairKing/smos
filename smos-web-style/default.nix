{ mkDerivation, autoexporter, base, lib, path, path-io, process
, template-haskell, yesod-static
}:
mkDerivation {
  pname = "smos-web-style";
  version = "0.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base path path-io process template-haskell yesod-static
  ];
  libraryToolDepends = [ autoexporter ];
  license = lib.licenses.mit;
}

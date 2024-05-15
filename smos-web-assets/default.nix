{ mkDerivation, autoexporter, base, lib, template-haskell
, yesod-static
}:
mkDerivation {
  pname = "smos-web-assets";
  version = "0.0.2";
  src = ./.;
  libraryHaskellDepends = [ base template-haskell yesod-static ];
  libraryToolDepends = [ autoexporter ];
  license = lib.licenses.mit;
}

{ mkDerivation
, base
, deepseq
, QuickCheck
, random
, stdenv
, tasty
, tasty-hunit
, tasty-quickcheck
, unix
}:
mkDerivation {
  pname = "time";
  version = "1.10";
  sha256 = "1000fhgcvqagvyhm587zf6y9g2ffj517w0hsvvpbzj1sggyjc93s";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    base
    deepseq
    QuickCheck
    random
    tasty
    tasty-hunit
    tasty-quickcheck
    unix
  ];
  homepage = "https://github.com/haskell/time";
  description = "A time library";
  license = stdenv.lib.licenses.bsd3;
}

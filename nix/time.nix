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
, fetchFromGitHub
}:
mkDerivation {
  pname = "time";
  version = "1.10";
  src = fetchFromGitHub {
    owner = "haskell";
    repo = "time";
    rev = "8ffb3da1118ddd40cbb2bc3cd8cf4a9d94d15211";
    sha256 = "sha256:0b8d0k5igc2s1mhlzb7bnjxknq6bj1qvqs2az3s9nlwkcifcb2rf";
  };
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

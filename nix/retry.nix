{ mkDerivation, base, exceptions, fetchgit, ghc-prim, hedgehog
, HUnit, mtl, random, stdenv, stm, tasty, tasty-hedgehog
, tasty-hunit, time, transformers
}:
mkDerivation {
  pname = "retry";
  version = "0.8.0.0";
  src = fetchgit {
    url = "https://github.com/Soostone/retry.git";
    sha256 = "06b44zgjqhkcvxaaqplb0nxmdj3sdp0v3cydrq0f8256wwkb9b0s";
    rev = "320fa9225909342672df5d6851c2f358e1be3c3a";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base exceptions ghc-prim random transformers
  ];
  testHaskellDepends = [
    base exceptions ghc-prim hedgehog HUnit mtl random stm tasty
    tasty-hedgehog tasty-hunit time transformers
  ];
  homepage = "http://github.com/Soostone/retry";
  description = "Retry combinators for monadic actions that may fail";
  license = stdenv.lib.licenses.bsd3;
}

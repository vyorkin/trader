{ mkDerivation, base, containers, eventful-core
, eventful-test-helpers, fetchgit, hpack, hspec, HUnit, mtl, safe
, stdenv, stm
}:
mkDerivation {
  pname = "eventful-memory";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/jdreaver/eventful.git";
    sha256 = "1v2bar9x4z3193wwxq7yw5iaabj8i5b7p3vxxnrplz2fqadmanfd";
    rev = "6b9ea9f6e4c5687ce7fc214ee5e459d405641a08";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/eventful-memory; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers eventful-core mtl safe stm
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base containers eventful-core eventful-test-helpers hspec HUnit mtl
    safe stm
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/jdreaver/eventful#readme";
  description = "In-memory implementations for eventful";
  license = stdenv.lib.licenses.mit;
}

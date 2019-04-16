{ mkDerivation, aeson, base, containers, contravariant, fetchgit
, hpack, hspec, http-api-data, HUnit, path-pieces, stdenv
, sum-type-boilerplate, template-haskell, text, transformers, uuid
}:
mkDerivation {
  pname = "eventful-core";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/jdreaver/eventful.git";
    sha256 = "1v2bar9x4z3193wwxq7yw5iaabj8i5b7p3vxxnrplz2fqadmanfd";
    rev = "6b9ea9f6e4c5687ce7fc214ee5e459d405641a08";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/eventful-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base containers contravariant http-api-data path-pieces
    sum-type-boilerplate template-haskell text transformers uuid
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base containers contravariant hspec http-api-data HUnit
    path-pieces sum-type-boilerplate template-haskell text transformers
    uuid
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/jdreaver/eventful#readme";
  description = "Core module for eventful";
  license = stdenv.lib.licenses.mit;
}

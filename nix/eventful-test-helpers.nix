{ mkDerivation, aeson, aeson-casing, base, eventful-core, extra
, fetchgit, hpack, hspec, monad-logger, stdenv, text
}:
mkDerivation {
  pname = "eventful-test-helpers";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/jdreaver/eventful.git";
    sha256 = "1v2bar9x4z3193wwxq7yw5iaabj8i5b7p3vxxnrplz2fqadmanfd";
    rev = "6b9ea9f6e4c5687ce7fc214ee5e459d405641a08";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/eventful-test-helpers; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson aeson-casing base eventful-core extra hspec monad-logger text
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/jdreaver/eventful#readme";
  description = "Common module used for eventful tests";
  license = stdenv.lib.licenses.mit;
}

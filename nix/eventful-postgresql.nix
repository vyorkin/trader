{ mkDerivation, aeson, base, bytestring, eventful-core
, eventful-sql-common, eventful-test-helpers, fetchgit, hpack
, hspec, HUnit, mtl, persistent, persistent-postgresql, stdenv
, text
}:
mkDerivation {
  pname = "eventful-postgresql";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/jdreaver/eventful.git";
    sha256 = "1v2bar9x4z3193wwxq7yw5iaabj8i5b7p3vxxnrplz2fqadmanfd";
    rev = "6b9ea9f6e4c5687ce7fc214ee5e459d405641a08";
    fetchSubmodules = true;
  };
  doCheck = false;
  postUnpack = "sourceRoot+=/eventful-postgresql; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring eventful-core eventful-sql-common mtl
    persistent text
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring eventful-core eventful-sql-common
    eventful-test-helpers hspec HUnit mtl persistent
    persistent-postgresql text
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/jdreaver/eventful#readme";
  description = "Postgres implementations for eventful";
  license = stdenv.lib.licenses.mit;
}

{ mkDerivation, aeson, authenticate-oauth, base, blaze-builder
, bytestring, case-insensitive, connection, fetchgit, hspec
, hspec-core, hspec-discover, http-api-data, http-client
, http-client-tls, http-types, monad-control, mtl, QuickCheck
, retry, stdenv, text, time, transformers, transformers-base
, unordered-containers
}:
mkDerivation {
  pname = "req";
  version = "2.0.1";
  src = fetchgit {
    url = "https://github.com/mrkkrp/req.git";
    sha256 = "0yxsl6gl1is69x5gyv8129zzlipsk33jgj39k01sv7ma3yw2ix33";
    rev = "4df31fc7178b5a04fff02c9deaeda9d35739d677";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson authenticate-oauth base blaze-builder bytestring
    case-insensitive connection http-api-data http-client
    http-client-tls http-types monad-control mtl retry text time
    transformers transformers-base
  ];
  testHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive hspec
    hspec-core http-client http-types monad-control mtl QuickCheck
    retry text time unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/mrkkrp/req";
  description = "Easy-to-use, type-safe, expandable, high-level HTTP client library";
  license = stdenv.lib.licenses.bsd3;
}

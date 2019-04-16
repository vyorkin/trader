{ mkDerivation, aeson, base, base16-bytestring, bytestring, co-log
, containers, cryptonite, data-default-class, dhall, directory
, doctest, dotenv, eventful-core, eventful-memory
, eventful-postgresql, eventful-test-helpers, hedgehog
, http-api-data, http-client, http-types, lens, lens-aeson, memory
, natural-transformation, optparse-applicative, persistent
, persistent-postgresql, relude, req, stdenv, stm
, sum-type-boilerplate, tasty, tasty-discover
, tasty-expected-failure, tasty-hedgehog, text, time, unliftio-core
}:
mkDerivation {
  pname = "trader";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring co-log containers
    cryptonite data-default-class dhall eventful-core eventful-memory
    eventful-postgresql http-api-data http-client http-types lens
    lens-aeson memory natural-transformation persistent
    persistent-postgresql relude req stm sum-type-boilerplate text time
    unliftio-core
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring co-log containers
    cryptonite data-default-class dhall directory dotenv eventful-core
    eventful-memory eventful-postgresql http-api-data http-client
    http-types lens lens-aeson memory natural-transformation
    optparse-applicative persistent persistent-postgresql relude req
    stm sum-type-boilerplate text time unliftio-core
  ];
  testHaskellDepends = [
    aeson base base16-bytestring bytestring co-log containers
    cryptonite data-default-class dhall doctest eventful-core
    eventful-memory eventful-postgresql eventful-test-helpers hedgehog
    http-api-data http-client http-types lens lens-aeson memory
    natural-transformation persistent persistent-postgresql relude req
    stm sum-type-boilerplate tasty tasty-expected-failure
    tasty-hedgehog text time unliftio-core
  ];
  testToolDepends = [ tasty-discover ];
  description = "A simple trading bot";
  license = stdenv.lib.licenses.mit;
}

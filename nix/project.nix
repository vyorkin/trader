{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, bytestring, co-log, containers, cryptonite, data-default-class
, dhall, directory, doctest, dotenv, eventful-core, eventful-memory
, eventful-postgresql, eventful-test-helpers, exceptions, hedgehog
, http-api-data, http-client, http-types, lens, lens-aeson, memory
, monad-logger, natural-transformation, optparse-applicative
, persistent, persistent-postgresql, relude, req, stdenv, stm
, sum-type-boilerplate, suspend, tasty, tasty-discover
, tasty-expected-failure, tasty-hedgehog, template-haskell, text
, time, timers, unliftio-core
}:
mkDerivation {
  pname = "trader";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base base16-bytestring bytestring co-log
    containers cryptonite data-default-class dhall eventful-core
    eventful-memory eventful-postgresql exceptions http-api-data
    http-client http-types lens lens-aeson memory
    natural-transformation persistent persistent-postgresql relude req
    stm sum-type-boilerplate suspend text time timers unliftio-core
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base base16-bytestring bytestring co-log
    containers cryptonite data-default-class dhall directory dotenv
    eventful-core eventful-memory eventful-postgresql exceptions
    http-api-data http-client http-types lens lens-aeson memory
    monad-logger natural-transformation optparse-applicative persistent
    persistent-postgresql relude req stm sum-type-boilerplate suspend
    template-haskell text time timers unliftio-core
  ];
  testHaskellDepends = [
    aeson aeson-pretty base base16-bytestring bytestring co-log
    containers cryptonite data-default-class dhall doctest
    eventful-core eventful-memory eventful-postgresql
    eventful-test-helpers exceptions hedgehog http-api-data http-client
    http-types lens lens-aeson memory natural-transformation persistent
    persistent-postgresql relude req stm sum-type-boilerplate suspend
    tasty tasty-expected-failure tasty-hedgehog text time timers
    unliftio-core
  ];
  testToolDepends = [ tasty-discover ];
  description = "A simple trading bot";
  license = stdenv.lib.licenses.mit;
}

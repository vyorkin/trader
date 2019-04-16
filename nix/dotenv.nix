{ mkDerivation, base, base-compat, containers, directory
, exceptions, fetchgit, hspec, hspec-megaparsec, megaparsec
, optparse-applicative, process, stdenv, text, transformers, yaml
}:
mkDerivation {
  pname = "dotenv";
  version = "0.7.0.0";
  src = fetchgit {
    url = "https://github.com/stackbuilders/dotenv-hs.git";
    sha256 = "10gm2n13h41r08gsaxwyp7dlkrg9s0nrzml6izkavwx8fkqd2fdp";
    rev = "8a3712274fe6889b029e2a2154a34d78dc6d082c";
    fetchSubmodules = true;
  };
  doCheck = false;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base base-compat containers directory exceptions megaparsec process
    text transformers yaml
  ];
  executableHaskellDepends = [
    base base-compat megaparsec optparse-applicative process text
    transformers yaml
  ];
  testHaskellDepends = [
    base base-compat containers directory exceptions hspec
    hspec-megaparsec megaparsec process text transformers yaml
  ];
  homepage = "https://github.com/stackbuilders/dotenv-hs";
  description = "Loads environment variables from dotenv files";
  license = stdenv.lib.licenses.mit;
}

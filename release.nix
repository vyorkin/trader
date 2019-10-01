{ compiler ? "ghc865" }:

let
  overrides = {
    dontCheck = ["co-log"];
    doJailbreak = [];
    dontHaddock = [];
    dontCoverage = [];
    dontBenchmark = [];
  };

  generatedOverrides = haskellPackages: haskellPackagesOld:
  let
    toPackage = file: _: {
      name = builtins.replaceStrings [".nix"] [""] file;
      value = haskellPackages.callPackage (./. + "/nix/${file}") { };
    };
  in pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

  makeOverrides = function: names: haskellPackages: haskellPackagesOld:
  let
    toPackage = name: {
      inherit name;
      value = function haskellPackagesOld.${name};
    };
  in builtins.listToAttrs (map toPackage names);

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: { });

  # More exotic overrides go here
  manualOverrides = haskellPackagesNew: haskellPackagesOld: { };

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck overrides.dontCheck)
              (makeOverrides pkgs.haskell.lib.doJailbreak overrides.doJailbreak)
              (makeOverrides pkgs.haskell.lib.dontHaddock overrides.dontHaddock)
              (makeOverrides pkgs.haskell.lib.dontHaddock
              overrides.dontCoverage)
              (makeOverrides pkgs.haskell.lib.dontHaddock
              overrides.dontBenchmark)
              manualOverrides
            ];
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in { project = pkgs.haskell.packages."${compiler}".project; }

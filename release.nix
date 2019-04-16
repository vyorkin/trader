let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project = haskellPackagesNew.callPackage ./project.nix { };

          req = haskellPackagesNew.callPackage ./nix/req.nix { };
          retry = haskellPackagesNew.callPackage ./nix/retry.nix { };
          dotenv = haskellPackagesNew.callPackage ./nix/dotenv.nix { };

          eventful-core = haskellPackagesNew.callPackage ./nix/eventful-core.nix { };
          eventful-memory = haskellPackagesNew.callPackage ./nix/eventful-memory.nix { };
          eventful-postgresql = haskellPackagesNew.callPackage ./nix/eventful-postgresql.nix { };
          eventful-test-helpers = haskellPackagesNew.callPackage ./nix/eventful-test-helpers.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.project;
  }

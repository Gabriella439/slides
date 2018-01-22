let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: {
          bears = haskellPackagesNew.callPackage ./bears.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithPackages (haskellPackages: [
    haskellPackages.bears
    haskellPackages.bytestring
    haskellPackages.cassava
    haskellPackages.containers
    haskellPackages.diagrams
    haskellPackages.diagrams-svg
    haskellPackages.discrimination
    haskellPackages.lens
    haskellPackages.text
    haskellPackages.vector
  ]);

in
  pkgs.stdenv.mkDerivation {
    name = "empty";

    buildCommand = "touch $out";

    buildInputs = [ ghc ];
  }

let
  pkgs = import <nixpkgs> { };

  ghc = pkgs.haskellPackages.ghcWithPackages (haskellPackages: [
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

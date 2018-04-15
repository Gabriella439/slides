let
  pkgs = import <nixpkgs> { config = import ./config.nix; };

in
  pkgs.haskellPackages.simple-ci.env

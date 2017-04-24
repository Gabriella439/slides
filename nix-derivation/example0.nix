let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "example0";

    buildCommand = ''
      touch $out
    '';
  }

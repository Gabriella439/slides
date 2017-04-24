let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "impure";

    buildCommand = ''
      date > $out
    '';
  }

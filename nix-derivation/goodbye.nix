let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "hello.txt";

    buildCommand = ''
      echo 'Goodbye, world!' > $out
    '';
  }

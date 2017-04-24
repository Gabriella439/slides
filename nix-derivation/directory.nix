let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "directory";

    buildCommand = ''
      mkdir $out
      echo 'Hello!'   > $out/hello.txt
      echo 'Goodbye!' > $out/goodbye.txt
    '';
  }

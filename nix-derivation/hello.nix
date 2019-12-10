# ./hello.nix

let
  pkgs = import <nixpkgs> { };

in
  # This is our first time using `pkgs.stdenv.mkDerivation`
  pkgs.stdenv.mkDerivation {
    name = "hello";

    src = pkgs.fetchurl {
      url = "mirror://gnu/hello/2.10.tar.gz";
      sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
    };
  }

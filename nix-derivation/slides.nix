let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "slides";

    buildCommand = ''
      mkdir $out
      ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./slides.md} -o $out/slides.html
    '';
  }

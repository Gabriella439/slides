# ./slides.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "slides.html" { } ''
    ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./slides.md} -o $out
  ''

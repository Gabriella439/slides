# ./slides.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "slides.html" {} ''
    mkdir $out

    ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./slides.md} -o $out/slides.html

    cp ${./awake-blue.png} $out/awake-blue.png
  ''

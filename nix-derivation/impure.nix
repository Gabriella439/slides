# ./impure.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "impure" { } ''
    date > $out
  ''

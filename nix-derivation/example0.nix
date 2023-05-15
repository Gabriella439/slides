# ./example0.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "example0" { } ''
    touch $out
  ''

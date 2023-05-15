# ./file.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "hello.txt" { } ''
    echo 'Goodbye, world!' > $out
  ''

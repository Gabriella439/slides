# ./directory.nix

let
  pkgs = import <nixpkgs> { };

in
  pkgs.runCommand "directory" {} ''
    mkdir $out

    echo 'Hello!' > $out/hello.txt

    echo 'Goodbye!' > $out/goodbye.txt
  ''

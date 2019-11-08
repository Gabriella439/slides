let
  pkgs = import <nixpkgs> { };

in
  pkgs.myEnvFun {
    name = "foo";

    buildInputs = [
      pkgs.cabal-install
      pkgs.go
      pkgs.sbt
    ];
  }

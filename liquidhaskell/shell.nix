let
  inherit (import <nixpkgs> { }) fetchFromGitHub;

  nixpkgs = fetchFromGitHub {
    owner = "NixOS";

    repo = "nixpkgs";

    rev = "1715436b75696d9885b345dd8159e12244d0f7f5";
    sha256 = "18qp76cppm1yxmzdaak9kcllbypvv22c9g7iaycq2wz0qkka6rx5";
  };

  pkgs = import nixpkgs { };

  liquid =
    pkgs.runCommand "liquidhaskell" { buildInputs = [ pkgs.makeWrapper ]; } ''
      mkdir -p $out/bin
      ln -s ${pkgs.haskellPackages.liquidhaskell}/bin/liquid $out/bin
      wrapProgram $out/bin/liquid --prefix PATH : ${pkgs.z3}/bin
    '';

  ghc = pkgs.haskellPackages.ghcWithPackages (packages: with packages; [
    vector
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "my-haskell-env-0";
    buildInputs = [ ghc liquid ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }

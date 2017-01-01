let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: {
          bears = haskellPackagesNew.callPackage ./bears.nix { };

          foldl = haskellPackagesNew.callPackage ./foldl.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { category =
      pkgs.runCommand "category.html" {} ''
        mkdir -p $out
        ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./category.md} -o $out/category.html
      '';

    data =
      pkgs.runCommand "data.html" {} ''
        mkdir -p $out
        ln -s ${./cucumber0.jpeg} $out/cucumber0.jpeg
        ln -s ${./chart0.svg}     $out/chart0.svg
        ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./data.md} -o $out/data.html
      '';

    bears = pkgs.runCommand "bears-haddocks" {} ''
      ln -s ${pkgs.haskellPackages.bears}/share/doc/x86_64-linux-ghc-8.0.1/bears-1.0.0/html $out
    '';
  }

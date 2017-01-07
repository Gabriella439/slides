let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: {
          bears = haskellPackagesNew.callPackage ./bears.nix { };

          examples = haskellPackagesNew.callPackage ./examples { };

          foldl = haskellPackagesNew.callPackage ./foldl.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { category =
      pkgs.runCommand "category-slides" {} ''
        mkdir -p $out
        ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./category.md} -o $out/category.html
      '';

    examples = pkgs.haskellPackages.examples;

    data =
      let
        flmnhSmall =
          pkgs.runCommand "flmnh-small" {} ''
            ${pkgs.imagemagick}/bin/convert -resize 30% ${./flmnh.jpg} $out
          '';
      in
        pkgs.runCommand "data-slides" {} ''
          mkdir -p $out
          ln -s ${./cucumber0.jpeg} $out/cucumber0.jpeg
          ln -s ${./cucumber1.jpg}  $out/cucumber1.jpg
          ln -s ${./cucumber2.jpg}  $out/cucumber2.jpg
          ln -s ${flmnhSmall}       $out/flmnh.jpg
          ln -s ${./chart0.svg}     $out/chart0.svg
          ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./data.md} -o $out/data.html
        '';

    bears = pkgs.runCommand "bears-haddocks" {} ''
      ln -s ${pkgs.haskellPackages.bears}/share/doc/x86_64-linux-ghc-8.0.1/bears-1.0.0/html $out
    '';
  }

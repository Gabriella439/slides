let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: {
          examples = haskellPackagesNew.callPackage ./examples { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { category =
      pkgs.runCommand "category-slides" {} ''
        mkdir -p $out
        ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./category.md} -o $out/slides.html
      '';

    examples = pkgs.haskellPackages.examples;
  }

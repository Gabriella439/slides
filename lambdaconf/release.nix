let
  pkgs = import <nixpkgs> { };

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
  }

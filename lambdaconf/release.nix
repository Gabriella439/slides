let
  pkgs = import <nixpkgs> { };

in
  { category =
      pkgs.runCommand "category.html" {} ''
        ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./category.md} -o $out
      '';

    data =
      pkgs.runCommand "data.html" {} ''
        ${pkgs.pandoc}/bin/pandoc -t slidy -s ${./data.md} -o $out
      '';
  }

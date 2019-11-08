let
  pkgs = import <nixpkgs> { };

in
  { foo = { bar = pkgs.stdenv.mkDerivation {
      name = "hello";

      buildCommand = ''
        mkdir -p $out/bin
        cat > $out/bin/hello <<EOF
          echo "Hello, world!"
        EOF
        chmod u+x $out/bin/hello
      '';
    }; };

    goodbye = pkgs.stdenv.mkDerivation {
      name = "goodbye";

      buildCommand = ''
        mkdir -p $out/bin
        cat > $out/bin/goodbye <<EOF
          echo "Goodbye, world!"
        EOF
        chmod u+x $out/bin/goodbye
      '';
    };
  }

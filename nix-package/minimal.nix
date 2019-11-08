let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "hello";

    buildCommand = ''
      mkdir -p $out/bin
      cat > $out/bin/hello <<EOF
        echo "Hello, world!"
      EOF
      chmod u+x $out/bin/hello
    '';
  }

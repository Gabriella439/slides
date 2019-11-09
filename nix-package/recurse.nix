let
  pkgs = import <nixpkgs> { };

  hello = pkgs.stdenv.mkDerivation {
    name = "hello";

    buildCommand = ''
      mkdir -p $out/bin
      cat > $out/bin/hello <<EOF
        echo "Hello, world!"
      EOF
      chmod u+x $out/bin/hello
    '';
  };

in
  pkgs.recurseIntoAttrs {
    foo = pkgs.recurseIntoAttrs {
      bar = pkgs.recurseIntoAttrs {
        baz = hello;
      };
    };
  }

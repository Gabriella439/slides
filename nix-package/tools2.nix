let
  pkgs = import <nixpkgs> { };

  makeScript = packageName: greeting: pkgs.stdenv.mkDerivation {
    name = packageName;

    buildCommand = ''
      mkdir -p $out/bin
      cat > $out/bin/${packageName} <<EOF
        echo "${greeting}"
      EOF
      chmod u+x $out/bin/${packageName}
    '';
  };

in
  { hello = makeScript "hello" "Hello, world!";

    goodbye = makeScript "goodbye" "Goodbye, world!";
  }

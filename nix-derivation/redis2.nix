let
  pkgs = import <nixpkgs> { };

in
  pkgs.stdenv.mkDerivation {
    name = "redis";

    makeFlags = "PREFIX=$(out)";

    src = pkgs.fetchFromGitHub {
      owner = "antirez";

      repo = "redis";

      rev = "27fe8e9fb2f4adf5337e74280215680e7cd59442";

      sha256 = null;
    };
  }

let
  pkgs = import <nixpkgs> { };

  json = builtins.fromJSON (builtins.readFile ./redis.json);
in
  pkgs.stdenv.mkDerivation {
    name = "redis";

    makeFlags = "PREFIX=$(out)";

    src = pkgs.fetchFromGitHub {
      owner = "antirez";

      repo = "redis";

      rev = json.rev;

      sha256 = json.sha256;
    };
  }

{ mkDerivation, aeson, base, http-client, http-client-tls, servant
, servant-client, servant-server, stdenv, text, transformers, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "simple-ci";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base http-client http-client-tls servant servant-client
    servant-server text transformers wai wai-extra warp
  ];
  license = stdenv.lib.licenses.bsd3;
}

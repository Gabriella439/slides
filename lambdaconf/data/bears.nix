{ mkDerivation, base, bytestring, cassava, containers, doctest
, fetchgit, foldl, stdenv, template-haskell, transformers, vector
}:
mkDerivation {
  pname = "bears";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Bears-Library.git";
    sha256 = "1wmb7zzkds353lzb1xmyk5rn6ynkcqhmnfvwv0br9r3g77pzcjml";
    rev = "65ff68fe02a9375aaf5567a3231eaeb0b028bf8e";
  };
  libraryHaskellDepends = [
    base bytestring cassava containers foldl template-haskell
    transformers vector
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "http://github.com/Gabriel439/Haskell-Bears-Library";
  description = "Relational algebra";
  license = stdenv.lib.licenses.bsd3;
}

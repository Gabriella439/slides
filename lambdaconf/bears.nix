{ mkDerivation, base, bytestring, cassava, containers, doctest
, fetchgit, foldl, stdenv, template-haskell, transformers, vector
}:
mkDerivation {
  pname = "bears";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Bears-Library.git";
    sha256 = "1xcj7aszqjn5i26947hxzmkscwrfaxhf7251v1393jgi6bh9wk7i";
    rev = "88619ae4056f18e9cd11f708e83977fb73ef8e86";
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

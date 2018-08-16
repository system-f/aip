{ mkDerivation, aeson, aeson-pretty, base, bytestring, checkers
, Crypto, directory, exceptions, filepath, HTTP, lens, network-uri
, optparse-applicative, papa, parsec, parsers, QuickCheck, stdenv
, tagsoup, tagsoup-selection, tasty, tasty-hunit, tasty-quickcheck
, time, transformers, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "aip";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring Crypto directory exceptions
    filepath HTTP lens network-uri optparse-applicative papa parsec
    parsers tagsoup tagsoup-selection time transformers
    unordered-containers utf8-string
  ];
  executableHaskellDepends = [ base papa ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

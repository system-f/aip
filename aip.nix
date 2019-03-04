{ mkDerivation, aeson, aeson-pretty, base, bytestring, checkers
, Crypto, directory, exceptions, filepath, HTTP, lens, network-uri
, optparse-applicative, parsec, parsers, process, QuickCheck
, semigroupoids, semigroups, stdenv, tagsoup, tagsoup-selection
, tasty, tasty-hunit, tasty-quickcheck, time, transformers
, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "aip";
  version = "0.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring Crypto directory exceptions
    filepath HTTP lens network-uri optparse-applicative parsec parsers
    process semigroupoids semigroups tagsoup tagsoup-selection time
    transformers unordered-containers utf8-string
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

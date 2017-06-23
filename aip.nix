{ mkDerivation, base, bytestring, digit, directory, doctest
, filepath, HTTP, network-uri, papa, parsec, parsers, QuickCheck
, quickcheck-text, stdenv, tagsoup, tagsoup-selection
, template-haskell, transformers
}:
mkDerivation {
  pname = "aip";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring digit directory filepath HTTP network-uri papa
    parsec parsers tagsoup tagsoup-selection transformers
  ];
  testHaskellDepends = [
    base directory doctest filepath parsec QuickCheck quickcheck-text
    template-haskell
  ];
  homepage = "https://github.com/tonymorris/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

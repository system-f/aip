{ mkDerivation, base, bytestring, Crypto, digit, directory, doctest
, filepath, HTTP, lens, network-uri, papa, parsec, parsers
, QuickCheck, quickcheck-text, stdenv, sys-process, tagsoup
, tagsoup-selection, template-haskell, time, transformers
}:
mkDerivation {
  pname = "aip";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring digit directory filepath HTTP lens network-uri papa
    parsec parsers tagsoup tagsoup-selection transformers
  ];
  executableHaskellDepends = [
    base bytestring Crypto digit directory filepath HTTP lens
    network-uri papa parsec parsers sys-process tagsoup
    tagsoup-selection time transformers
  ];
  testHaskellDepends = [
    base directory doctest filepath parsec QuickCheck quickcheck-text
    template-haskell
  ];
  homepage = "https://github.com/qfpl/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

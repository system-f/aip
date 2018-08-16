{ mkDerivation, base, bytestring, checkers, Crypto, digit
, directory, exitcode, filepath, HTTP, lens, network-uri, papa
, parsec, parsers, process, QuickCheck, stdenv, tagsoup
, tagsoup-selection, tasty, tasty-hunit, tasty-quickcheck, time
, transformers, utf8-string
}:
mkDerivation {
  pname = "aip";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring Crypto digit directory filepath HTTP lens
    network-uri papa parsec parsers tagsoup tagsoup-selection
    transformers utf8-string
  ];
  executableHaskellDepends = [
    base bytestring Crypto digit directory exitcode filepath HTTP lens
    network-uri papa parsec parsers process tagsoup tagsoup-selection
    time transformers utf8-string
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

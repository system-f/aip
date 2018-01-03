{ mkDerivation, base, bomradar, bytestring, checkers, digit
, directory, filepath, HTTP, lens, network-uri, papa, parsec
, parsers, QuickCheck, stdenv, sys-process, tagsoup
, tagsoup-selection, tasty, tasty-hunit, tasty-quickcheck, time
, transformers
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
    base bytestring digit directory filepath HTTP lens network-uri papa
    parsec parsers sys-process tagsoup tagsoup-selection time
    transformers
  ];
  testHaskellDepends = [
    base bomradar checkers lens QuickCheck tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

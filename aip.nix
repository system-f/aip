{ mkDerivation, aeson, aeson-pretty, base, bytestring, checkers
, Crypto, digit, directory, exceptions, exitcode, filepath, HTTP
, iso8601-time, lens, network-uri, papa, parsec, parsers, process
, QuickCheck, stdenv, tagsoup, tagsoup-selection, tasty
, tasty-hunit, tasty-quickcheck, text, time, transformers, unix
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "aip";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring Crypto digit directory
    exceptions filepath HTTP iso8601-time lens network-uri papa parsec
    parsers tagsoup tagsoup-selection text time transformers unix
    unordered-containers utf8-string vector
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring Crypto digit directory
    exceptions exitcode filepath HTTP iso8601-time lens network-uri
    papa parsec parsers process tagsoup tagsoup-selection text time
    transformers unix unordered-containers utf8-string vector
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/aip";
  description = "Aeronautical Information Package (AIP)";
  license = stdenv.lib.licenses.bsd3;
}

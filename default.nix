{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "9752ec3341df8117121c23e2fa8eadc38af7841b";
      sha256 = "05g00hjd8hi5lvc9k9smqh6s4sjyd07hr94qicjsigl5k241gibh";
    };

    digit = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "digit";
      rev = "df2cf23cbd74c07809a394c52c3decb51dd06438";
      sha256 = "0hsgr2xngvdaqi5n8sg080ifl64b94xm2mw87jwnx55i7gq1mkic";
    };

  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      digit = import sources.digit { inherit nixpkgs compiler; };
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      tagsoup-selection = pkgs.haskell.lib.doJailbreak super.tagsoup-selection;
    };
  };

  aip = modifiedHaskellPackages.callPackage ./aip.nix {};

in
  aip

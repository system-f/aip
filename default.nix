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
      rev = "536b0a9243802347c299e077b5d85beb80d3a4a1";
      sha256 = "10wx0z5cd8dajr3rdskaq64v42ppa8dbb3rs3jyj872218xjz6nr";
    };

    notzero = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "notzero";
      rev = "04fbbe14773166de273577c0a6cb8dd89358fc78";
      sha256 = "0ypad68l7017my3vhcids21wx27lm381xx52c9q8pwviqlvdd077";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      notzero = import sources.notzero { inherit nixpkgs compiler; };
      tagsoup-selection = pkgs.haskell.lib.doJailbreak super.tagsoup-selection;
    };
  };

  aip = modifiedHaskellPackages.callPackage ./aip.nix {};

in
  aip

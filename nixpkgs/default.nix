let

  inherit (import <nixpkgs> {}) fetchFromGitHub;

  pkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "9ed5dbafe35d4cc9d1ddfdab2ceea971bd999662";
    sha256 = "0phxbzaqd0xmkmiy04kyyfikff7g0bd9ikqnyagy8wflwx7bm0gi";
  }) {};

  llvm-hs-repo = pkgs.fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs";
    rev = "76cd4d5107862401a7ebbe1bb9cc1cf172fa1d66";
    sha256 = "0bnh0yyjflhvc8vjrqsa25k7issnvkvgx149bnq7avka5mx2m99m";
  };

  overlay = self: super: with self; {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: with self; {
        llvm-hs-pure = callPackage (import "${llvm-hs-repo}/llvm-hs-pure") {};
        llvm-hs = callPackage (import "${llvm-hs-repo}/llvm-hs") { llvm-config = llvm_4; };
      };
    };

  };

in

import pkgs.path { overlays = [ overlay ]; }

let

  inherit (import <nixpkgs> {}) fetchFromGitHub;

  pkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "3ccc139b3db8d9417006eb9deeb75038b2fc33fa";
    sha256 = "1fzbkqlni45hsx313c6cnh4apwzsqasv3vmgp7ih20lhmxv9w5sj";
  }) {};

  overlay = self: super: with self; {

    llvmPackages_4 = callPackage ./llvm_4 {
      inherit (stdenvAdapters) overrideCC;
    };
    llvm_4 = llvmPackages_4.llvm;
    clang_4 = llvmPackages_4.clang;

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: with self; {
        llvm-hs-pure = callPackage ./llvm-hs-pure {};
        llvm-hs = callPackage ./llvm-hs { llvm-config = llvm_4; };
      };
    };

  };

in

import pkgs.path { overlays = [ overlay ]; }

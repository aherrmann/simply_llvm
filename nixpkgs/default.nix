let

  inherit (import <nixpkgs> {}) fetchFromGitHub;

  pkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "46683187069bbeceeb70455c55c08f26d2a67bc2";
    sha256 = "1435g12390grfkadjp25ypyimmdqrjxv71q7bbzmrilhbpfp2yg8";
  }) {};

  overlay = self: super: with self; {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: with self; {
        llvm-hs-pure = callPackage ./llvm-hs-pure {};
        llvm-hs = callPackage ./llvm-hs { llvm-config = llvm_4; };
      };
    };

  };

in

import pkgs.path { overlays = [ overlay ]; }

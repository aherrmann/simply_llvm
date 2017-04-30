let

  inherit (import <nixpkgs> {}) fetchFromGitHub;

  pkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "9b948ea439ddbaa26740ce35543e7e35d2aa6d18";
    sha256 = "17208in65j29xsipzfdy5hi0nmqf836i28bkcihh18qys7669bj8";
  }) {};

  llvm-hs-repo = pkgs.fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs";
    rev = "76cd4d5107862401a7ebbe1bb9cc1cf172fa1d66";
    sha256 = "0bnh0yyjflhvc8vjrqsa25k7issnvkvgx149bnq7avka5mx2m99m";
  };

  llvm-hs-pretty-repo = pkgs.fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs-pretty";
    rev = "aeed01e4abc8d28ec1c0b681b55ebc4d0a15d5df";
    sha256 = "0bblf21jwii2h61g46zgdrw5s0yxjj24m42d0akfp92x8y7yjcnw";
  };

  overlay = self: super: with self; {

    haskellPackages = super.haskellPackages.override {
      overrides = self: super: with self; {
        hedgehog = callPackage ./hedgehog {};
        hlint = hlint_2_0_9;
        llvm-hs-pure = callPackage (import "${llvm-hs-repo}/llvm-hs-pure") {};
        llvm-hs = callPackage (import "${llvm-hs-repo}/llvm-hs") { llvm-config = llvm_4; };
        llvm-hs-pretty = callPackage (import llvm-hs-pretty-repo) {};
        prettyprinter-ansi-terminal = callPackage ./prettyprinter-ansi-terminal {};
      };
    };

    llvm_4 = (super.llvm_4.override { debugVersion = true; }).overrideAttrs (attrs:
      let
        shlib = if stdenv.isDarwin then "dylib" else "so";
        inherit (self) stdenv;
        release_version = "4.0.0";
        shortVersion = with stdenv.lib;
          concatStringsSep "." (take 2 (splitString "." release_version))
        ;
      in
      {
        # Need to fix broken `postInstall` phase in debug mode.
        # See https://github.com/NixOS/nixpkgs/pull/26429
        postInstall = ''
          moveToOutput "share/man" "$man"
        ''
        + stdenv.lib.optionalString true ''
          moveToOutput "lib/libLLVM-*" "$lib"
          moveToOutput "lib/libLLVM.${shlib}" "$lib"
          substituteInPlace "$out/lib/cmake/llvm/LLVMExports-debug.cmake" \
            --replace "\''${_IMPORT_PREFIX}/lib/libLLVM-" "$lib/lib/libLLVM-"
        ''
        + stdenv.lib.optionalString (stdenv.isDarwin && enableSharedLibraries) ''
          substituteInPlace "$out/lib/cmake/llvm/LLVMExports-debug.cmake" \
            --replace "\''${_IMPORT_PREFIX}/lib/libLLVM.dylib" "$lib/lib/libLLVM.dylib"
          install_name_tool -id $lib/lib/libLLVM.dylib $lib/lib/libLLVM.dylib
          install_name_tool -change @rpath/libLLVM.dylib $lib/lib/libLLVM.dylib $out/bin/llvm-config
          ln -s $lib/lib/libLLVM.dylib $lib/lib/libLLVM-${shortVersion}.dylib
          ln -s $lib/lib/libLLVM.dylib $lib/lib/libLLVM-${release_version}.dylib
        '';
      })
    ;

  };

in

import pkgs.path { overlays = [ overlay ]; }

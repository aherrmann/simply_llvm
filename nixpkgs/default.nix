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
    rev = "4f11240a630fb0be7f250f9926dd8fdbedb121c3";
    sha256 = "12lmv2vrsyc1ynfy2cvc2miqn6izzacackygg1zid82jcpi10xmj";
  };

  llvm-hs-pretty-repo = pkgs.fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs-pretty";
    rev = "bb56f12e348aa1e19ec6111cecd200d4e0ffced9";
    sha256 = "1zfgmnxygi3gchd06zg4ar3l2nf94jkv73h2ahklzscvlbg7lmkj";
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

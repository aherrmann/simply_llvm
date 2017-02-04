{ mkDerivation, fetchFromGitHub
, base, containers, HUnit, mtl, parsec, QuickCheck
, stdenv, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers, transformers-compat
}:
mkDerivation {
  pname = "llvm-hs-pure";
  version = "4.0.0.0";
  src = builtins.toPath "${fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs";
    rev = "b7e6c8520f3eb51d000526d93d06f5e9b00cb3d8";
    sha256 = "09pjpf4afifq4ic3lshjwc7fhh5w1axmx92ffs5zy90q5frs5pr4";
  }}/llvm-hs-pure";
  libraryHaskellDepends = [
    base containers mtl parsec template-haskell transformers
    transformers-compat
  ];
  testHaskellDepends = [
    base containers HUnit mtl QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
    transformers-compat
  ];
  doHaddock = false;
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}

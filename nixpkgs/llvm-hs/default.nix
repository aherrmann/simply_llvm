{ mkDerivation, fetchFromGitHub
, array, base, bytestring, Cabal, containers, HUnit
, llvm-config, llvm-hs-pure, mtl, parsec, QuickCheck, stdenv
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers, transformers-compat
, utf8-string
}:
mkDerivation {
  pname = "llvm-hs";
  version = "4.0.0.0";
  src = builtins.toPath "${fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs";
    rev = "b7e6c8520f3eb51d000526d93d06f5e9b00cb3d8";
    sha256 = "09pjpf4afifq4ic3lshjwc7fhh5w1axmx92ffs5zy90q5frs5pr4";
  }}/llvm-hs";
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array base bytestring containers llvm-hs-pure mtl parsec
    template-haskell transformers transformers-compat utf8-string
  ];
  libraryToolDepends = [ llvm-config ];
  testHaskellDepends = [
    base containers HUnit llvm-hs-pure mtl QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
    transformers-compat
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}

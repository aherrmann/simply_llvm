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
    rev = "a06007debe3448466454363ec8a0823381edbd87";
    sha256 = "1sqmjjbvf0g1l6rm6jbvjdvh5xzfby5kqmjx3g2967p10x0pndlh";
  }}/llvm-hs";
  configureFlags = [ "-fshared-llvm" ];
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

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
    rev = "a06007debe3448466454363ec8a0823381edbd87";
    sha256 = "1sqmjjbvf0g1l6rm6jbvjdvh5xzfby5kqmjx3g2967p10x0pndlh";
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
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}

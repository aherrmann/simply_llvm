with (import ./nixpkgs);

let
  ghc =
    haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
      ansi-wl-pprint
      dlist
      GenericPretty
      hedgehog
      hspec
      libffi
      llvm-hs
      llvm-hs-pretty
      llvm-hs-pure
      managed
      pretty
      protolude
    ]);
in
pkgs.stdenv.mkDerivation {
  name = "simply-llvm-env";
  buildInputs = [
    cabal-install ghc llvm_4 clang_4
    hlint
  ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

with (import ./nixpkgs);

let
  ghc =
    haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
      dlist
      hedgehog
      hspec
      libffi
      llvm-hs
      llvm-hs-pretty
      llvm-hs-pure
      managed
      megaparsec
      prettyprinter
      prettyprinter-ansi-terminal
      protolude
      show-prettyprint
    ]);
in
pkgs.stdenv.mkDerivation {
  name = "simply-llvm-env";
  buildInputs = [
    cabal-install hlint ghc llvm_4 clang_4
  ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

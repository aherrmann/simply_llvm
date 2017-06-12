with (import ./nixpkgs);

let
  ghc =
    haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
      protolude
      dlist
      llvm-hs-pure
      llvm-hs
      libffi
      ansi-wl-pprint
      pretty
      GenericPretty
    ]);
in
pkgs.stdenv.mkDerivation {
  name = "simply-llvm-env";
  buildInputs = [
    cabal-install ghc llvm_4 clang_4
  ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

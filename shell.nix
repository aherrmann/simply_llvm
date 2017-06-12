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
    PS1='nix:\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]$(__git_ps1 "(%s)")\$ '
  '';
}

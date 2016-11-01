# Simply LLVM

Compiling a simple functional language with LLVM and Haskell

Example code to the presentation given at the HaskellerZ Zurich Haskell meetup.


## Setup

If you have the Nix package manager installed on your machine,
then you just need to execute the following command in the top-level
directory of this repository:

``` sh
$ nix-shell
```


## Usage

Once the environment is set up you can enter an interactive GHCi session with

``` sh
$ cabal repl
```

and start exploring the code:

``` hs
λ> ex01a_factorial & Simply.typeCheck >>= pp
λ> ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform >>= pp
λ> ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= printLLVM
λ> ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= printLLVMOpt optInline
λ> ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= printAssemblyOpt optInline
λ> ex01b_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= exec [5]
```

You should have a look at the [slides](slides/slides.md)
to get an overview over the code.

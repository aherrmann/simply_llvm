# Simply LLVM

Compiling a simple functional language with LLVM and Haskell

Example code to the presentation given at the HaskellerZ Zurich Haskell meetup.
Ported to use the [`llvm-hs`](https://github.com/llvm-hs/llvm-hs)
LLVM Haskell bindings.


## Setup

If you have the Nix package manager installed on your machine,
then you just need to execute the following command in the top-level
directory of this repository:

``` sh
$ nix-shell
```


## Usage

Once the environment is set up you can enter an interactive GHCi session with either:

``` sh
$ cabal repl
$ stack repl
```

and start exploring the code:

``` hs
λ> ex01a_factorial & Surface.prettyPrint
λ> ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface >>= prettyPrint
λ> ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= printLLVM
λ> ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= printLLVMOpt optInline
λ> ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= printAssemblyOpt optInline
λ> ex01b_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= exec [5]
```

The examples can be loaded by in the interactive session with:

``` sh
λ> :load Example.hs
λ> ex5
```

Example programs in the language *Simply* are provided in [`simply_examples`](simply_examples).
You can compile and run them as follows:

``` sh
$ cabal run simply simply_examples/factorial.simply factorial
$ ./factorial
$ ./factorial 5
```

You should have a look at the [slides](slides/slides.md)
to get an overview over the code.

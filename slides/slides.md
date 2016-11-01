---
title: >
    Compiling a simple functional language<br>
    with LLVM and Haskell
author: >
    Andreas Herrmann<br>
    <a href="https://github.com/aherrmann/simply_llvm">
        <i class="fa fa-github" aria-hidden="true"></i>
        github.com/aherrmann/simply_llvm
    </a>
date: November 24, 2016
css:
    - https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css
---


# What is LLVM

<img alg="LLVM logo" style="height:300px; float:right;" src="http://llvm.org/img/DragonMedium.png">

- Compiler toolchain

    * native code for multiple targets
        (x86, x86_64, ARM, and many more)

    * target independent optimizer

    * just in time (JIT) compiler

- Abstract assembly language

    * infinite virtual registers

    * static single assignment (SSA) form


# Who uses LLVM

- Clang (part of LLVM) <img alg="LLVM logo" style="height:100px; vertical-align: middle;" src="http://llvm.org/img/DragonMedium.png">

- Rust <img alt="Rust logo" style="height:100px; vertical-align: middle;" src="https://www.rust-lang.org/logos/rust-logo-64x64.png">

- Julia <img alt="Julia logo" style="height:100px; vertical-align: middle;" src="http://docs.julialang.org/en/release-0.5/_static/julia-logo.svg">

- GHC <img alt="Haskell logo" style="height:100px; vertical-align: middle;" src="https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png">

- many more


# Demo LLVM IR

- [`01_addmul.ll`](../ir_examples/01_addmul.ll)

- [`02_factorial.ll`](../ir_examples/02_factorial.ll)


# Today's language: "Simply"

- Simply-typed lambda calculus

- Base types: `Int`, `Bool`; and function types: `a -> b`

- Literals, Variables

- Branches

- Primitive operations: `+`, `-`, `*`, `==`

- Lambda abstraction, and function application

- non-recursive let-bindings

- mutually recursive global bindings

- one main function


# Today's language - Example

Calculate the factorial of the given number

```
globals

    factorial' (acc : Int) (n : Int) : Int =
        if n == 0 then
            acc
        else
            factorial' (acc * n) (n - 1)

    factorial (n : Int) : Int =
        factorial' 1 n

main (n : Int) =
    factorial n
```


# Today's language - Supports

- higher order functions

- closures

- functions as values

```
globals

    apply (f : Int -> Int) (x : Int) : Int =
        f x

    mkAdder (a : Int) : Int -> Int =
        (+) a

main (n : Int) =
    let
        add3 = mkAdder 3
    in
        apply add3 n
```


# Compiler Pipeline

- Parse to AST (not implemented)

- Type-check

- Convert to intermediate representation

- Convert to LLVM IR

- (Optimize)

- Generate native code

- Execute


# Abstract Syntax Tree

- ["Simply" AST](../src/Simply/AST/Simply.hs)

- [Type Checker](../src/Simply/TypeCheck/Simply.hs)

- [Examples](../src/Simply/Examples/Simply.hs)


# Motivate IR

Difference between "Simply" and LLVM:

- In LLVM everything is explicitly typed (annotated AST)

- LLVM distinguishes local and global variables explicitly

- LLVM only allows global function definitions

- LLVM has no closures

- In LLVM all function have to be fully applied


# Intermediate Representation

- [Intermediate representation AST](../src/Simply/AST/IR.hs)

- [Transform "Simply" to IR](../src/Simply/Transform/Simply2IR.hs)

    (Up to closures)

- [Examples](../src/Simply/Examples/Simply.hs)


# Demo llvm-general-pure

- [`Manual.hs`](../manual-llvm/Manual.hs)


# Show Codegen.hs

- [`Codegen.hs`](../src/Simply/LLVM/Codegen.hs)


# Transform to LLVM

- [Transform IR to LLVM](../src/Simply/Transform/IR2LLVM.hs)

    (Up to closures)

- [JIT](../src/Simply/LLVM/JIT.hs)

- [Examples](../src/Simply/Examples/Simply.hs)


# Closures on the Machine

- Closure captures environment and expects arguments

- Global function that takes environment and arguments

- [Example in C](../ir_examples/03_closure.c)


# Closures in IR

- [Intermediate representation AST](../src/Simply/AST/IR.hs)

- [Transform "Simply" to IR](../src/Simply/Transform/Simply2IR.hs)

- [Examples](../src/Simply/Examples/Simply.hs)


# Closures in LLVM

- [Transform IR to LLVM](../src/Simply/Transform/IR2LLVM.hs)

- [Examples](../src/Simply/Examples/Simply.hs)


# Where to go from here?

- Parsing, Pretty-Printing, Testing
    * [Dave Laing](http://dlaing.org/little-languages/)

- Garbage Collection
    * [LLVM documentation](http://llvm.org/docs/GarbageCollection.html)

- Polymorphism (Damas–Hindley–Milner)
    * [Stephen Diehl](https://github.com/sdiehl/paris-fp/blob/master/src/Infer.hs)
    * [David Luposchainsky](https://github.com/quchen/articles/tree/master/hindley-milner#readme)

- Dependent Types
    * [Andres Löh, et al.](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)

- Backend for Morte
    * [Gabriel Gonzalez](http://www.haskellforall.com/2014/09/morte-intermediate-language-for-super.html)


# Thanks for your attention


# References

- [Kaleidoscope in Haskell by Stephen Diehl](http://www.stephendiehl.com/llvm/)

- [Building Small Native Languages with Haskell by Stephen Diehl](http://dev.stephendiehl.com/paris.pdf)
    ([code](https://github.com/sdiehl/paris-fp))
    ([video](https://youtu.be/I51TRpl1qic))

- [Closure Conversion by Jeremy Siek](http://siek.blogspot.ch/2012/07/essence-of-closure-conversion.html?m=1)

- [LLVM Bindings by Benjamin S. Scarlet](http://hackage.haskell.org/package/llvm-general-pure)

- [LLVM Language Reference](http://llvm.org/releases/3.5.0/docs/LangRef.html)

- [Types and Programming Languages by Benjamin C. Pierce](https://books.google.ch/books?id=ti6zoAC9Ph8C&redir_esc=y&hl=en)

- [Practical Foundations for Programming Languages by Robert Harper](http://www.cs.cmu.edu/~rwh/pfpl.html)

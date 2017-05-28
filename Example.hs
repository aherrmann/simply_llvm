{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Protolude
import Text.PrettyPrint.GenericPretty (pp)

import Simply.LLVM.JIT
import Simply.AST.Simply
import Simply.Examples.Simply
import qualified Simply.AST.IR as IR
import qualified Simply.TypeCheck.Simply as Simply
import qualified Simply.Transform.Simply2IR as Simply2IR
import qualified Simply.Transform.IR2LLVM as IR2LLVM

ex1, ex2, ex3, ex4, ex5, ex6 :: IO ()
ex1 = ex01a_factorial & Simply.typeCheck >>= pp
ex2 = ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform >>= pp
ex3 = ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= printLLVM
ex4 = ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= printLLVMOpt optInline
ex5 = ex01a_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= printAssemblyOpt optInline
ex6 = ex01b_factorial & Simply.typeCheck & fmap Simply2IR.transform & fmap IR2LLVM.transform >>= exec [5]

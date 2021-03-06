{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main (module Main) where

import Protolude

import Text.Show.Prettyprint

import Simply.Surface.AST
import Simply.LLVM.JIT
import Simply.Examples
import qualified Simply.Surface.Pretty as Surface
import qualified Simply.Surface.TypeCheck as Simply
import qualified Simply.Intermediate.AST as Intermediate
import qualified Simply.Intermediate.FromSurface as Intermediate
import qualified Simply.LLVM.FromIntermediate as LLVM

ex1, ex2, ex3, ex4, ex5 :: IO ()
ex1 = ex01a_factorial & Surface.prettyPrint
ex2 = ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface >>= prettyPrint
ex3 = ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= printLLVM
ex4 = ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= printLLVMOpt optInline
ex5 = ex01a_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= printAssemblyOpt optInline
ex6 :: IO Int32
ex6 = ex01b_factorial & Simply.typeCheck' & fmap Intermediate.fromSurface & fmap LLVM.fromIntermediate >>= exec [5]

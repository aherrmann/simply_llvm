{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Simply where

import Protolude hiding (Type)

import Text.PrettyPrint.GenericPretty (pp)

import Simply.Surface.AST
import qualified Simply.Intermediate.AST as Intermediate
import qualified Simply.Surface.TypeCheck as Simply
import qualified Simply.Intermediate.FromSurface as Intermediate
import qualified Simply.LLVM.FromIntermediate as LLVM
import Simply.LLVM.JIT
import Simply.Examples

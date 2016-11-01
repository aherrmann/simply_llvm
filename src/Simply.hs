module Simply where

import Protolude
import Text.PrettyPrint.GenericPretty (pp)
import Simply.AST.Simply
import qualified Simply.AST.IR as IR
import qualified Simply.TypeCheck.Simply as Simply
import qualified Simply.Transform.Simply2IR as Simply2IR
import qualified Simply.Transform.IR2LLVM as IR2LLVM
import Simply.LLVM.JIT
import Simply.Examples.Simply

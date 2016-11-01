{-# LANGUAGE NoImplicitPrelude #-}

module Codegen where

import Protolude
import LLVM.General.AST
import qualified LLVM.General.AST as AST
import LLVM.General.AST.Global
import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.PrettyPrint


int :: Type
int = IntegerType 32


defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter int (Name "a") []
        , Parameter int (Name "b") [] ]
      , False )
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference int (Name "a"))
                (LocalReference int (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference int (Name "result"))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "simply-llvm"
  , moduleDefinitions = [defAdd]
  }


toLLVM mod = withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ withModuleFromAST ctx mod moduleLLVMAssembly
    case errOrLLVM of
      Left err -> putStrLn $ "error: " ++ err
      Right llvm -> putStrLn llvm


-- $ ghci Codegen.hs
--
-- > toLLVM module_

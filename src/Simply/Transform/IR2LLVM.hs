module Simply.Transform.IR2LLVM
  ( transform
  ) where

import Protolude hiding (local, void, zero)
import Data.List (zip3)

import Simply.AST.IR as IR
import Simply.LLVM.Codegen

import LLVM.General.AST.Type
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP
import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.PassManager
import LLVM.General.PrettyPrint
import LLVM.General.Target


----------------------------------------------------------------------
-- Interface

-- | Transform IR to LLVM.
--
-- Every type-correct "Simply" program can be transformed to LLVM through IR.
--
-- This function is partial and will fail on an ill-formed program.
transform :: Program -> AST.Module
transform prog = runLLVM initModule (codegenProg prog)
  where
    initModule = emptyModule "Simply-LLVM Module"


----------------------------------------------------------------------
-- Transformations

-- | Transform an IR type to an LLVM type.
llvmType :: IR.Type -> AST.Type
llvmType TInt = int
llvmType TBool = ibool
llvmType (TFunction args ret) = fn (llvmType ret) (map llvmType args)
llvmType (TClosure args ret) = closure (llvmType ret) (map llvmType args)


-- | Transform an IR expression to LLVM.
codegenExpr :: Expr -> Codegen AST.Operand
codegenExpr expr = case expr of

  Lit TInt (LInt x) ->
    pure $! cint (fromIntegral x)

  Lit TBool (LBool x) ->
    pure $! bool false true x
  
  LVar ty x ->
    getvar (toS x)

  GVar ty x ->
    pure $! cons $ global (llvmType ty) (AST.Name $ toS x)

  Let ty name ebound ein -> do
    ebound' <- codegenExpr ebound
    assign (toS name) ebound'
    codegenExpr ein

  If ty cond tr fl  -> do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    -- %entry
    ------------------
    cond <- codegenExpr cond
    test <- icmp IP.EQ true cond
    cbr test ifthen ifelse

    -- if.then
    ------------------
    setBlock ifthen
    trval <- codegenExpr tr
    br ifexit
    ifthen <- getBlock

    -- if.else
    ------------------
    setBlock ifelse
    flval <- codegenExpr fl
    br ifexit
    ifelse <- getBlock

    -- if.exit
    ------------------
    setBlock ifexit
    phi int [(trval, ifthen), (flval, ifelse)]

  Prim TInt Add [a,b] ->
    join $! add <$> codegenExpr a <*> codegenExpr b

  Prim TInt Sub [a,b] ->
    join $! sub <$> codegenExpr a <*> codegenExpr b

  Prim TInt Mul [a,b] ->
    join $! mul <$> codegenExpr a <*> codegenExpr b

  Prim TBool Eql [a,b] ->
    join $! icmp IP.EQ <$> codegenExpr a <*> codegenExpr b

  CallFunction retTy name args -> do
    let funTy = llvmType (TFunction (map exprType args) retTy)
    args' <- traverse codegenExpr args
    call (llvmType retTy) (externf funTy (AST.Name (toS name))) args'

  MakeClosure (TClosure argtys retty) name env -> do
    envptr' <-
        if null env then
            pure $! nullP anyPtr
        else do
            let envtys' = map (llvmType . exprType) env
            env' <- traverse codegenExpr env
            (anyptr, envptr) <- malloc (closureEnv envtys')
            forM_ (zip3 [0..] envtys' env') $ \(i, ty, v) -> do
                p <- getElementPtr (ptr ty) envptr [cint32 0, cint32 i]
                store p v
            pure $! anyptr
    let retty' = llvmType retty
        argtys' = map llvmType argtys
        closurety = closure retty' argtys'
        funty = fn retty' (anyPtr : argtys')
        name' = AST.Name $ toS name
    buildStruct closurety [cons $ global funty name', envptr']

  CallClosure retty cl args -> do
    let TClosure clargtys clretty = exprType cl
    unless (clargtys == map exprType args)
      $ panic "Argument type mismatch in call closure"
    unless (retty == clretty)
      $ panic "Return type mismatch in call closure"

    let funty = fn (llvmType retty) (anyPtr : map (llvmType . exprType) args)
    cl' <- codegenExpr cl
    args' <- traverse codegenExpr args
    funPtr <- extractValue funty cl' [0]
    envPtr <- extractValue anyPtr cl' [1]
    call (llvmType retty) funPtr (envPtr : args')

  _ -> panic $! "Invalid expression: " <> show expr 


-- | transform a global binding to LLVM
--
-- adds global definitons to the LLVM module
codegenGlobal :: Global -> LLVM ()
codegenGlobal (DefFunction name args retty body) = do
  let args' = map (swap . first (AST.Name . toS) . second llvmType) args
      retty' = llvmType retty
  internal retty' (toS name) args' $
    codegenBody args' body
codegenGlobal (DefClosure name env args retty body) = do
  let env' = map (swap . first (AST.Name . toS) . second llvmType) env
      args' = map (swap . first (AST.Name . toS) . second llvmType) args
      retty' = llvmType retty
      envname = "__env"
      envarg = (anyPtr, envname)
  internal retty' (toS name) (envarg:args') $ do

    -- extract closure environment
    unless (null env) $ do
      envptr <- getClosureEnvPtr envname (map fst env')
      forM_ (zip [0..] env') $ \(i, (ty, AST.Name name)) -> do
          p <- getElementPtr (ptr ty) envptr [cint32 0, cint32 i]
          v <- load ty p
          assign (toS name) v

    codegenBody args' body


-- | Transform a functions to LLVM
-- that expects the given arguments in contex.
codegenBody
  :: [(AST.Type, AST.Name)] -> Expr -> Codegen (AST.Named AST.Terminator)
codegenBody args' body = do
    -- extract argument list
    forM_ args' $ \(ty, AST.Name name) ->
       assign name (local ty (AST.Name name))

    body' <- codegenExpr body
    ret body'


-- | transform a program to LLVM
codegenProg :: Program -> LLVM ()
codegenProg (Program glbls args retty main) = do

  -- external symbols
  external (ptr i8) "malloc" [(i32, "size")]

  -- global functions
  mapM_ codegenGlobal glbls

  -- main function
  let args' = map (swap . first (AST.Name . toS) . second llvmType) args
      retty' = llvmType retty
  define retty' "__main" args' $
    codegenBody args' main

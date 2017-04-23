module Simply.LLVM.FromIntermediate
  ( fromIntermediate
  ) where

import Protolude hiding (Type, local, void, zero)

import Data.List (zip3)

import LLVM.AST.Type
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.IntegerPredicate as IP

import Simply.Intermediate.AST as Intermediate
import Simply.LLVM.Codegen


----------------------------------------------------------------------
-- Interface

-- | Transform IR to LLVM.
--
-- Every type-correct "Simply" program can be transformed to LLVM through IR.
--
-- This function is partial and will fail on an ill-formed program.
fromIntermediate :: Program -> LLVM.Module
fromIntermediate prog = runLLVM initModule (codegenProg prog)
  where initModule = emptyModule "Simply-LLVM Module"


----------------------------------------------------------------------
-- Transformations

-- | Transform an IR type to an LLVM type.
llvmType :: Intermediate.Type -> LLVM.Type
llvmType TInt = int
llvmType TBool = ibool
llvmType (TFunction args retty) = fn (llvmType retty) (map llvmType args)
llvmType (TClosure args retty) = closure (llvmType retty) (map llvmType args)


-- | Transform an IR expression to LLVM.
codegenExpr :: Expr -> Codegen LLVM.Operand
codegenExpr expr = case expr of

  Lit TInt (LInt x) ->
    pure $! cint (fromIntegral x)

  Lit TBool (LBool x) ->
    pure $! bool false true x

  LVar _ty x ->
    getvar (toS x)

  GVar ty x ->
    pure $! cons $ global (llvmType ty) (LLVM.Name $ toS x)

  Let _ty name ebound ein -> do
    ebound' <- codegenExpr ebound
    assign (toS name) ebound'
    codegenExpr ein

  If _ty cond tr fl  -> do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    -- %entry
    ------------------
    condition <- codegenExpr cond
    test <- icmp IP.EQ true condition
    _ <- cbr test ifthen ifelse

    -- if.then
    ------------------
    _ <- setBlock ifthen
    trval <- codegenExpr tr
    _ <- br ifexit
    ifthen' <- getBlock

    -- if.else
    ------------------
    _ <- setBlock ifelse
    flval <- codegenExpr fl
    _ <- br ifexit
    ifelse' <- getBlock

    -- if.exit
    ------------------
    _ <- setBlock ifexit
    phi int [(trval, ifthen'), (flval, ifelse')]

  BinaryOp TInt Add a b ->
    join $! add <$> codegenExpr a <*> codegenExpr b

  BinaryOp TInt Sub a b ->
    join $! sub <$> codegenExpr a <*> codegenExpr b

  BinaryOp TInt Mul a b ->
    join $! mul <$> codegenExpr a <*> codegenExpr b

  BinaryOp TBool Eql a b ->
    join $! icmp IP.EQ <$> codegenExpr a <*> codegenExpr b

  CallFunction retTy name args -> do
    let funTy = llvmType (TFunction (map exprType args) retTy)
    args' <- traverse codegenExpr args
    call (llvmType retTy) (externf funTy (LLVM.Name (toS name))) args'

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
    let
      retty' = llvmType retty
      argtys' = map llvmType argtys
      closurety = closure retty' argtys'
      funty = fn retty' (anyPtr : argtys')
      name' = LLVM.Name $ toS name
    buildStruct closurety [cons $ global funty name', envptr']

  CallClosure retty cl args -> do
    let TClosure clargtys clretty = exprType cl
    unless (clargtys == map exprType args) $
      panic "Argument type mismatch in call closure"
    unless (retty == clretty) $
      panic "Return type mismatch in call closure"

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
  let
    args' = map (swap . first (LLVM.Name . toS) . second llvmType) args
    retty' = llvmType retty
    def = if name == "main" then define else internal
  def retty' (toS name) args' $
    codegenBody args' body
codegenGlobal (DefClosure name env args retty body) = do
  let
    env' = map (swap . first (LLVM.Name . toS) . second llvmType) env
    args' = map (swap . first (LLVM.Name . toS) . second llvmType) args
    retty' = llvmType retty
    envname = "__env"
    envarg = (anyPtr, envname)
  internal retty' (toS name) (envarg:args') $ do

    -- extract closure environment
    unless (null env) $ do
      envptr <- getClosureEnvPtr envname (map fst env')
      forM_ (zip [0..] env') $ \(i, (ty, LLVM.Name n)) -> do
        p <- getElementPtr (ptr ty) envptr [cint32 0, cint32 i]
        v <- load ty p
        assign (toS n) v

    codegenBody args' body


-- | Transform a functions to LLVM
-- that expects the given arguments in contex.
codegenBody
  :: [(LLVM.Type, LLVM.Name)] -> Expr -> Codegen (LLVM.Named LLVM.Terminator)
codegenBody args' body = do
  -- extract argument list
  forM_ args' $ \(ty, LLVM.Name name) ->
    assign (toS name) (local ty (LLVM.Name name))

  body' <- codegenExpr body
  ret body'


-- | transform a program to LLVM
codegenProg :: Program -> LLVM ()
codegenProg (Program glbls) = do

  -- external symbols
  external (ptr i8) "malloc" [(i32, "size")]

  -- global functions
  mapM_ codegenGlobal glbls

module Simply.LLVM.FromIntermediate
  ( fromIntermediate
  ) where

import Protolude hiding (Type, local, void, zero)

import Control.Exception (assert)
import Data.List (zip3)

import LLVM.AST.Type
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.AST.IntegerPredicate as IP

import Simply.Intermediate.AST as Intermediate
import Simply.LLVM.Codegen


assertM :: Applicative m => Bool -> m ()
assertM = flip assert $ pure ()


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


toFunPtrType :: LLVM.Type -> LLVM.Type
toFunPtrType ty@(FunctionType _ _ _) = ptr ty
toFunPtrType ty = ty


llvmValueType :: Intermediate.Type -> LLVM.Type
llvmValueType = toFunPtrType . llvmType


toFunPtr :: LLVM.Operand -> LLVM.Operand
toFunPtr (LLVM.LocalReference ty n) = LLVM.LocalReference (toFunPtrType ty) n
toFunPtr (LLVM.ConstantOperand (LLVM.GlobalReference ty n)) =
  LLVM.ConstantOperand (LLVM.GlobalReference (toFunPtrType ty) n)
toFunPtr op = op


-- | Transform an IR expression to LLVM.
codegenExpr :: Expr -> Codegen LLVM.Operand
codegenExpr expr = case expr of

  Lit TInt (LInt x) ->
    pure $! cint (fromIntegral x)

  Lit TBool (LBool x) ->
    pure $! bool false true x

  Var _ty x ->
    getvar (toS x)

  Global ty x ->
    pure $! cons $ global (llvmType ty) (LLVM.Name $ toS x)

  Let _ name ebound ein -> do
    ebound' <- codegenExpr ebound
    scope $ do
      assign (toS name) ebound'
      codegenExpr ein

  If ty cond then_ else_  -> do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    -- %entry
    ------------------
    cond' <- codegenExpr cond
    _ <- cbr cond' ifthen ifelse

    -- if.then
    ------------------
    _ <- setBlock ifthen
    then' <- codegenExpr then_
    _ <- br ifexit
    ifthen' <- getBlock

    -- if.else
    ------------------
    _ <- setBlock ifelse
    else' <- codegenExpr else_
    _ <- br ifexit
    ifelse' <- getBlock

    -- if.exit
    ------------------
    _ <- setBlock ifexit
    phi (llvmValueType ty) [(then', ifthen'), (else', ifelse')]

  BinaryOp TInt Add a b ->
    join $! add <$> codegenExpr a <*> codegenExpr b

  BinaryOp TInt Sub a b ->
    join $! sub <$> codegenExpr a <*> codegenExpr b

  BinaryOp TInt Mul a b ->
    join $! mul <$> codegenExpr a <*> codegenExpr b

  BinaryOp TBool Eql a b ->
    join $! icmp IP.EQ <$> codegenExpr a <*> codegenExpr b

  Call ty f args -> do

    let fty = exprType f
    assertM $ argTypes   fty == map exprType args
    assertM $ returnType fty == ty

    let ty' = llvmType ty
    f' <- codegenExpr f
    args' <- traverse codegenExpr args

    case fty of
      TFunction     _ _ -> call ty' f' args'
      TClosure argtys _ -> do
        let fty' = fn ty' (anyPtr : map llvmType argtys)
        fptr <- extractValue (ptr fty') f' [0]
        envptr <- extractValue anyPtr f' [1]
        call ty' fptr (envptr : args')
      _ -> panic $ "Attempt to call non-callable: " <> show fty

  Closure (TClosure argtys retty) name env -> do
    envptr' <-
      if null env then
        pure $! nullP anyPtr
      else do
        let envtys' = map (llvmValueType . exprType) env
        env' <- traverse codegenExpr env
        (anyptr, envptr) <- malloc (closureEnv envtys')
        forM_ (zip3 [0..] envtys' env') $ \(i, ty, v) -> do
          p <- getElementPtr (ptr ty) envptr [cint32 0, cint32 i]
          store p (toFunPtr v)
        pure $! anyptr
    let
      retty' = llvmType retty
      argtys' = map llvmType argtys
      closurety = closure retty' argtys'
      funty = fn retty' (anyPtr : argtys')
      name' = LLVM.Name $ toS name
    buildStruct closurety [cons $ global (ptr funty) name', envptr']

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
    env' = map (swap . first (LLVM.Name . toS) . second llvmValueType) env
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

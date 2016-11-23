{-# LANGUAGE TupleSections #-}

module Simply.Transform.Simply2IR
  ( transform
  ) where

import Protolude
import Control.Arrow ((&&&))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Simply.AST.Simply
import qualified Simply.AST.IR as IR


----------------------------------------------------------------------
-- Types

-- | the context maps names to types
type Context = Map Name IR.Type


-- | scope in which a name is defined
data Scope
  = GlobalScope
  | LocalScope
  deriving (Show, Eq, Ord)


-- | A transform reads the global and local context,
-- writes new global definitions,
-- and keeps a counter (state) for fresh variable names.
--
-- Transform a = Context -> Context -> Count -> (Count, Global, a)
type Transform = RWS (Context, Context) (DList IR.Global) Int


----------------------------------------------------------------------
-- Interface

-- | Transform "Simply" to IR.
--
-- Every type-correct "Simply" program can be transformed to IR.
--
-- It is illegal to pass a program that is not type-correct.
transform :: Program -> IR.Program
transform prog = IR.Program (DList.apply extra glbls) args retty main
  where
    (IR.Program glbls args retty main, extra) =
        evalRWS (transfProgram prog) (Map.empty, Map.empty) 0


----------------------------------------------------------------------
-- Context Handling

addGlobalCtx :: [(Name, IR.Type)] -> Transform a -> Transform a
addGlobalCtx ctx = local . first $ Map.union (Map.fromList ctx)

addLocalCtx :: [(Name, IR.Type)] -> Transform a -> Transform a
addLocalCtx ctx = local . second $ Map.union (Map.fromList ctx)

lookupGlobalCtx :: Name -> Transform (Maybe IR.Type)
lookupGlobalCtx name = asks $ Map.lookup name . fst

lookupLocalCtx :: Name -> Transform (Maybe IR.Type)
lookupLocalCtx name = asks $ Map.lookup name . snd

lookupCtx :: Name -> Transform (Maybe (IR.Type, Scope))
lookupCtx name = runMaybeT
   $  (, LocalScope) <$> MaybeT (lookupLocalCtx name)
  <|> (, GlobalScope) <$> MaybeT (lookupGlobalCtx name)


----------------------------------------------------------------------
-- State Handling

-- | produce a fresh name with the given prefix and bump the counter
freshname :: Text -> Transform Text
freshname pref = do
    n <- get
    modify (+1)
    pure $! pref <> show n


-- | add a global function definition
deffun :: Name -> [IR.Arg] -> IR.Type -> IR.Expr -> Transform ()
deffun name args retty body =
    tell $ DList.singleton (IR.DefFunction name args retty body)


----------------------------------------------------------------------
-- Transformations

-- | Transform a "Simply" type to an IR type.
transfType :: Type -> IR.Type
transfType = \case
    TInt -> IR.TInt
    TBool -> IR.TBool


-- | Assert that an expression has a certain type.
--
-- This exists because exceptions in Haskell are easier to debug
-- than segmentation faults, or failed assertions in C++ code.
assertType :: IR.Type -> IR.Expr -> Transform ()
assertType ty expr
  | IR.exprType expr == ty = pure ()
  | otherwise = panic "Type mismatch!"


-- | transform a "Simply" expression to IR
--
-- The function is partial and fails on ill-formed expressions.
transfExpr :: Expr -> Transform IR.Expr
transfExpr = \case

  Lit l@(LInt _) -> pure $! IR.Lit IR.TInt l
  Lit l@(LBool _) -> pure $! IR.Lit IR.TBool l

  Var name -> do
    mbCtx <- lookupCtx name
    case mbCtx of
      Just (ty, LocalScope) -> pure $! IR.LVar ty name
      Just (ty, GlobalScope) -> pure $! IR.GVar ty name
      _ -> panic "Unknown variable"

  Let name e ein -> do
    e' <- transfExpr e
    let ty = IR.exprType e'
    ein' <- addLocalCtx [(name, ty)] $ transfExpr ein
    let inty = IR.exprType ein'
    pure $! IR.Let inty name e' ein'

  If cond th el -> do
    cond' <- transfExpr cond
    th' <- transfExpr th
    el' <- transfExpr el
    pure $! IR.If (IR.exprType th') cond' th' el'

  Prim prim -> do
    let (pref, retty) = case prim of
          Add -> ("add", IR.TInt)
          Sub -> ("sub", IR.TInt)
          Mul -> ("mul", IR.TInt)
          Eql -> ("eql", IR.TBool)
    name <- freshname $ "__" <> pref <> "_"
    deffun name [("a", IR.TInt), ("b", IR.TInt)] retty $
      IR.Prim retty prim [IR.LVar IR.TInt "a", IR.LVar IR.TInt "b"]
    pure $! IR.GVar (IR.TFunction [IR.TInt, IR.TInt] retty) name

  app@(App _ _) ->
    case unfoldApp app of
      (f@(Var name), args) ->
        join $ call <$> transfExpr f <*> traverse transfExpr args
      (Prim prim, [a, b]) -> do
        let retty = if prim == Eql then IR.TBool else IR.TInt
        a' <- transfExpr a
        b' <- transfExpr b
        pure $! IR.Prim retty prim [a', b']
      (f, args) ->
        join $ call <$> transfExpr f <*> traverse transfExpr args


-- | Call an IR expression
--
-- Must either be a variable referring to
-- a global function, a local function.
--
-- The function is partial and fails if its pre-condition is violated.
call :: IR.Expr -> [IR.Expr] -> Transform IR.Expr
call f args = do
    let fty = IR.exprType f
        argtys = IR.argTypes fty
        retty = IR.returnType fty
        numargs = IR.numArgs fty
        numpassed = length args
    case compare numargs numpassed of
      EQ ->
        -- saturated call
        case f of
          IR.GVar (IR.TFunction _ _) name ->
            pure $! IR.CallFunction retty name args
          IR.LVar (IR.TFunction _ _) name ->
            panic "Can only call global functions!"


-- | transform a functions body in "Simply" to IR
-- that expects the given arguments in context and should produce the given
-- return type.
transfBody :: [IR.Arg] -> IR.Type -> Expr -> Transform IR.Expr
transfBody args' retty' body =
    addLocalCtx args' $ transfExpr body


-- | transform a global binding in "Simply" to IR
transfGlobal :: Global -> Transform IR.Global
transfGlobal (Def name args retty body) =
    IR.DefFunction name args' retty' <$> transfBody args' retty' body
  where
    args' = map (second transfType) args
    retty' = transfType retty


-- | transform a program in "Simply" to IR
transfProgram :: Program -> Transform IR.Program
transfProgram prog@(Program glbls args main) =
    addGlobalCtx ctx $ do
        glbls' <- traverse transfGlobal glbls
        let args' = map (second transfType) args
            retty' = IR.TInt -- Main function must always produce an Int
        main' <- transfBody args' retty' main
        pure $! IR.Program glbls' args' retty' main'
  where
    ctx = map (globalName &&& transfGlobalType) glbls
    transfGlobalType (Def _ args retty _) =
        IR.TFunction (map (transfType . snd) args) (transfType retty)

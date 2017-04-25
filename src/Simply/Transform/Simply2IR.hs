{-# LANGUAGE TupleSections #-}

module Simply.Transform.Simply2IR
  ( transform
  ) where

import Protolude hiding (Type)
import Control.Arrow ((&&&))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Maybe
import Control.Monad.RWS hiding ((<>))
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
transform prog = IR.Program (DList.apply extra glbls)
  where
    (IR.Program glbls, extra) =
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


-- | add a global closure definition
defclosure :: Name -> [IR.Arg] -> [IR.Arg] -> IR.Type -> IR.Expr -> Transform ()
defclosure name env args retty body =
    tell $ DList.singleton (IR.DefClosure name env args retty body)


-- | Lift an expression that expects the given arguments
-- and produces the given result type to a closure.
-- Capture all free local variables in the expression.
liftClosure :: [IR.Arg] -> IR.Type -> IR.Expr -> Transform IR.Expr
liftClosure args retty body = do
    name <- freshname "__closure_"
    let env = Map.toList $ IR.freeVars body Map.\\ Map.fromList args
        capture = map (uncurry $ flip IR.LVar) env
    defclosure name env args retty body
    pure $! IR.MakeClosure (IR.TClosure (map snd args) retty) name capture


----------------------------------------------------------------------
-- Transformations

-- | Transform a "Simply" type to an IR type.
transfType :: Type -> IR.Type
transfType = \case
    TInt -> IR.TInt
    TBool -> IR.TBool
    ty@(TArr _ _) ->
      let (argtys, retty) = unfoldTArr ty
      in  IR.TClosure (map transfType argtys) (transfType retty)


-- | Find a common type between two types that both types can be converted to.
--
-- E.g. a function without arguments can be converted to its result type.
--
-- The function is partial and fails if there is no common type. (Sorry)
promoteType :: IR.Type -> IR.Type -> IR.Type
promoteType a b =
    case (a, b) of
      (IR.TFunction [] r, _) | b == r -> r
      (IR.TClosure [] r, _) | b == r -> r
      (_, IR.TFunction [] r) | a == r -> r
      (_, IR.TClosure [] r) | a == r -> r
      _ | a == b -> a
        | otherwise -> panic "Invalid type promotion"


-- | Convert (if necessary) two expressions to their common type.
--
-- See 'promoteType'.
--
-- The function is partial and fails if there is no common type.
promote :: IR.Expr -> IR.Expr -> Transform (IR.Expr, IR.Expr)
promote a b = (,) <$> convert ty a <*> convert ty b
  where
    ty = promoteType (IR.exprType a) (IR.exprType b)


-- | Convert an expression to the given type.
--
-- The function is partial and fails if the conversion is illegal.
convert :: IR.Type -> IR.Expr -> Transform IR.Expr
convert ty expr =
    let ety = IR.exprType expr in
    case ety of
      -- Functions or closures without arguments
      -- can be implicitly converted to their return types
      -- by calling them.
      IR.TFunction [] retty | retty == ty -> call expr []
      IR.TClosure [] retty | retty == ty -> call expr []
      -- A function pointer can be converted to a closure
      -- as a partial application with zero arguments.
      IR.TFunction argtys retty | IR.TClosure argtys retty == ty ->
        partialCall expr []
      ty' | ty' == ty -> pure $! expr
      _ -> panic "Invalid convert"


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
    cond' <- convert IR.TBool =<< transfExpr cond
    (th', el') <- join $ promote <$> transfExpr th <*> transfExpr el
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
      (f@(Var _name), args) ->
        join $ call <$> transfExpr f <*> traverse transfExpr args
      (Prim prim, [a, b]) -> do
        let retty = if prim == Eql then IR.TBool else IR.TInt
        a' <- convert IR.TInt =<< transfExpr a
        b' <- convert IR.TInt =<< transfExpr b
        pure $! IR.Prim retty prim [a', b']
      (Prim prim, [a]) -> do
        let retty = if prim == Eql then IR.TBool else IR.TInt
        a' <- convert IR.TInt =<< transfExpr a
        liftClosure [("b", IR.TInt)] retty
            (IR.Prim retty prim [a', IR.LVar IR.TInt "b"])
      (lam@(Lam _ _), args) -> do
        f' <- transfExpr lam
        args' <- traverse transfExpr args
        call f' args'
      (f, args) ->
        join $ call <$> transfExpr f <*> traverse transfExpr args

  Lam args body -> do
    let args' = map (second transfType) args
    body' <- addLocalCtx args' $ transfExpr body
    let retty = IR.exprType body'
    liftClosure args' retty body'


-- | Call an IR expression
--
-- Must either be a variable referring to
-- a global function, a local function, or a closure;
-- or a closure expression.
--
-- The function is partial and fails if its pre-condition is violated.
call :: IR.Expr -> [IR.Expr] -> Transform IR.Expr
call f args = do
    let fty = IR.exprType f
        argtys = IR.argTypes fty
        retty = IR.returnType fty
        numargs = IR.numArgs fty
        numpassed = length args
    args' <- zipWithM convert argtys args
    case compare numargs numpassed of
      EQ ->
        -- saturated call
        case f of
          IR.GVar (IR.TFunction _ _) name ->
            pure $! IR.CallFunction retty name args'
          IR.LVar (IR.TFunction _ _) _name ->
            panic "Can only call global functions!"
          _ ->
            pure $! IR.CallClosure retty f args'
      LT -> do
        -- over saturated call - the call will itself return a function
        f' <- call f (take numargs args)
        call f' (drop numargs args)
      GT ->
        -- under saturated call - create a closure which will complete the call
        partialCall f args


-- | handle a partial function application
-- by creating a closure that expects the remaining arguments
partialCall :: IR.Expr -> [IR.Expr] -> Transform IR.Expr
partialCall fun args = do

    let funty = IR.exprType fun
        argtys = IR.argTypes funty
        retty = IR.returnType funty
        passed = length args
        missing = length argtys - passed

    args' <- zipWithM convert argtys args
    argnames <- replicateM missing (freshname "__arg_")

    let clargs = zip argnames $ drop passed argtys
        clexprs = map (uncurry $ flip IR.LVar) clargs

    join $! liftClosure clargs retty <$> call fun (args' ++ clexprs)


-- | transform a functions body in "Simply" to IR
-- that expects the given arguments in context and should produce the given
-- return type.
transfBody :: [IR.Arg] -> IR.Type -> Expr -> Transform IR.Expr
transfBody args' retty' body =
    addLocalCtx args' $ convert retty' =<< transfExpr body


-- | transform a global binding in "Simply" to IR
transfGlobal :: Global -> Transform IR.Global
transfGlobal (Def name args retty body) =
    IR.DefFunction name args' retty' <$> transfBody args' retty' body
  where
    args' = map (second transfType) args
    retty' = transfType retty


-- | transform a program in "Simply" to IR
transfProgram :: Program -> Transform IR.Program
transfProgram (Program glbls) =
    addGlobalCtx ctx $ do
        glbls' <- traverse transfGlobal glbls
        pure $! IR.Program glbls'
  where
    ctx = map (globalName &&& transfGlobalType) glbls
    transfGlobalType (Def _ args' retty _) =
        IR.TFunction (map (transfType . snd) args') (transfType retty)

{-# LANGUAGE TupleSections #-}

module Simply.Intermediate.FromSurface
  ( fromSurface
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

import Simply.Surface.AST (Name, BinaryOp (..), Lit (..))
import qualified Simply.Intermediate.AST as Intermediate
import qualified Simply.Surface.AST as Surface


----------------------------------------------------------------------
-- Types

-- | the context maps names to types
type Context = Map Name Intermediate.Type


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
type Transform = RWS (Context, Context) (DList Intermediate.Global) Int


----------------------------------------------------------------------
-- Interface

-- | Transform Surface AST to Intermediate AST.
--
-- Every type-correct "Simply" program can be transformed.
--
-- It is illegal to pass a program that is not type-correct.
fromSurface :: Surface.Program -> Intermediate.Program
fromSurface prog = Intermediate.Program (DList.apply extra glbls)
  where
    (Intermediate.Program glbls, extra) =
      evalRWS (transfProgram prog) (Map.empty, Map.empty) 0


----------------------------------------------------------------------
-- Context Handling

addGlobalCtx :: [(Name, Intermediate.Type)] -> Transform a -> Transform a
addGlobalCtx ctx = local . first $ Map.union (Map.fromList ctx)

addLocalCtx :: [(Name, Intermediate.Type)] -> Transform a -> Transform a
addLocalCtx ctx = local . second $ Map.union (Map.fromList ctx)

lookupGlobalCtx :: Name -> Transform (Maybe Intermediate.Type)
lookupGlobalCtx name = asks $ Map.lookup name . fst

lookupLocalCtx :: Name -> Transform (Maybe Intermediate.Type)
lookupLocalCtx name = asks $ Map.lookup name . snd

lookupCtx :: Name -> Transform (Maybe (Intermediate.Type, Scope))
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


-- | add a global closure definition
defclosure :: Name -> [Intermediate.Arg] -> [Intermediate.Arg] -> Intermediate.Type -> Intermediate.Expr -> Transform ()
defclosure name env args retty body =
  tell $ DList.singleton (Intermediate.DefClosure name env args retty body)


-- | Lift an expression that expects the given arguments
-- and produces the given result type to a closure.
-- Capture all free local variables in the expression.
liftClosure :: [Intermediate.Arg] -> Intermediate.Type -> Intermediate.Expr -> Transform Intermediate.Expr
liftClosure args retty body = do
  name <- freshname "__closure_"
  let
    env = Map.toList $ Intermediate.freeVars body Map.\\ Map.fromList args
    capture = map (uncurry $ flip Intermediate.LVar) env
  defclosure name env args retty body
  pure $! Intermediate.MakeClosure (Intermediate.TClosure (map snd args) retty) name capture


----------------------------------------------------------------------
-- Transformations

-- | Transform a Surface AST type to an Intermediate AST type.
transfType :: Surface.Type -> Intermediate.Type
transfType = \case
  Surface.TInt -> Intermediate.TInt
  Surface.TBool -> Intermediate.TBool
  ty@(Surface.TArr _ _) ->
    let (argtys, retty) = Surface.unfoldTArr ty in
    Intermediate.TClosure (map transfType argtys) (transfType retty)


-- | Find a common type between two types that both types can be converted to.
--
-- E.g. a function without arguments can be converted to its result type.
--
-- The function is partial and fails if there is no common type. (Sorry)
promoteType :: Intermediate.Type -> Intermediate.Type -> Intermediate.Type
promoteType a b =
  case (a, b) of
    (Intermediate.TFunction [] r, _) | b == r -> r
    (Intermediate.TClosure [] r, _) | b == r -> r
    (_, Intermediate.TFunction [] r) | a == r -> r
    (_, Intermediate.TClosure [] r) | a == r -> r
    _ | a == b -> a
      | otherwise -> panic "Invalid type promotion"


-- | Convert (if necessary) two expressions to their common type.
--
-- See 'promoteType'.
--
-- The function is partial and fails if there is no common type.
promote :: Intermediate.Expr -> Intermediate.Expr -> Transform (Intermediate.Expr, Intermediate.Expr)
promote a b = (,) <$> convert ty a <*> convert ty b
  where ty = promoteType (Intermediate.exprType a) (Intermediate.exprType b)


-- | Convert an expression to the given type.
--
-- The function is partial and fails if the conversion is illegal.
convert :: Intermediate.Type -> Intermediate.Expr -> Transform Intermediate.Expr
convert ty expr =
  let ety = Intermediate.exprType expr in
  case ety of
    -- Functions or closures without arguments
    -- can be implicitly converted to their return types
    -- by calling them.
    Intermediate.TFunction [] retty | retty == ty -> call expr []
    Intermediate.TClosure [] retty | retty == ty -> call expr []
    -- A function pointer can be converted to a closure
    -- as a partial application with zero arguments.
    Intermediate.TFunction argtys retty | Intermediate.TClosure argtys retty == ty ->
      partialCall expr []
    ty' | ty' == ty -> pure $! expr
    _ -> panic "Invalid convert"


-- | transform a Surface AST expression to an Intermediate AST expression
--
-- The function is partial and fails on ill-formed expressions.
transfExpr :: Surface.Expr -> Transform Intermediate.Expr
transfExpr = \case

  Surface.Lit l@(LInt _) -> pure $! Intermediate.Lit Intermediate.TInt l
  Surface.Lit l@(LBool _) -> pure $! Intermediate.Lit Intermediate.TBool l

  Surface.Var name -> do
    mbCtx <- lookupCtx name
    case mbCtx of
      Just (ty, LocalScope) -> pure $! Intermediate.LVar ty name
      Just (ty, GlobalScope) -> pure $! Intermediate.GVar ty name
      _ -> panic "Unknown variable"

  Surface.Let name e ein -> do
    e' <- transfExpr e
    let ty = Intermediate.exprType e'
    ein' <- addLocalCtx [(name, ty)] $ transfExpr ein
    let inty = Intermediate.exprType ein'
    pure $! Intermediate.Let inty name e' ein'

  Surface.If cond th el -> do
    cond' <- convert Intermediate.TBool =<< transfExpr cond
    (th', el') <- join $ promote <$> transfExpr th <*> transfExpr el
    pure $! Intermediate.If (Intermediate.exprType th') cond' th' el'

  Surface.BinaryOp op a b -> do
    let
      ty = case op of
        Add -> Intermediate.TInt
        Sub -> Intermediate.TInt
        Mul -> Intermediate.TInt
        Eql -> Intermediate.TBool
    a' <- convert Intermediate.TInt =<< transfExpr a
    b' <- convert Intermediate.TInt =<< transfExpr b
    pure $! Intermediate.BinaryOp ty op a' b'

  app@(Surface.App _ _) ->
    case Surface.unfoldApp app of
      (f@(Surface.Var _name), args) ->
        join $ call <$> transfExpr f <*> traverse transfExpr args
      (lam@(Surface.Lam _ _), args) -> do
        f' <- transfExpr lam
        args' <- traverse transfExpr args
        call f' args'
      (f, args) ->
        join $ call <$> transfExpr f <*> traverse transfExpr args

  Surface.Lam args body -> do
    let args' = map (second transfType) args
    body' <- addLocalCtx args' $ transfExpr body
    let retty = Intermediate.exprType body'
    liftClosure args' retty body'


-- | Construct a call
--
-- Must either be a variable referring to
-- a global function, a local function, or a closure;
-- or a closure expression.
--
-- The function is partial and fails if its pre-condition is violated.
call :: Intermediate.Expr -> [Intermediate.Expr] -> Transform Intermediate.Expr
call f args = do
  let
    fty = Intermediate.exprType f
    argtys = Intermediate.argTypes fty
    retty = Intermediate.returnType fty
    numargs = Intermediate.numArgs fty
    numpassed = length args
  args' <- zipWithM convert argtys args
  case compare numargs numpassed of
    EQ ->
      -- saturated call
      case f of
        Intermediate.GVar (Intermediate.TFunction _ _) name ->
          pure $! Intermediate.CallFunction retty name args'
        Intermediate.LVar (Intermediate.TFunction _ _) _name ->
          panic "Can only call global functions!"
        _ ->
          pure $! Intermediate.CallClosure retty f args'
    LT -> do
      -- over saturated call - the call will itself return a function
      f' <- call f (take numargs args)
      call f' (drop numargs args)
    GT ->
      -- under saturated call - create a closure which will complete the call
      partialCall f args


-- | handle a partial function application
-- by creating a closure that expects the remaining arguments
partialCall :: Intermediate.Expr -> [Intermediate.Expr] -> Transform Intermediate.Expr
partialCall fun args = do

  let
    funty = Intermediate.exprType fun
    argtys = Intermediate.argTypes funty
    retty = Intermediate.returnType funty
    passed = length args
    missing = length argtys - passed

  args' <- zipWithM convert argtys args
  argnames <- replicateM missing (freshname "__arg_")

  let
    clargs = zip argnames $ drop passed argtys
    clexprs = map (uncurry $ flip Intermediate.LVar) clargs

  join $! liftClosure clargs retty <$> call fun (args' ++ clexprs)


-- | transform a functions body in Surface AST to Intermediate AST
-- that expects the given arguments in context and should produce the given
-- return type.
transfBody :: [Intermediate.Arg] -> Intermediate.Type -> Surface.Expr -> Transform Intermediate.Expr
transfBody args' retty' body =
  addLocalCtx args' $ convert retty' =<< transfExpr body


-- | transform a global binding in Surface AST to Intermediate AST
transfGlobal :: Surface.Global -> Transform Intermediate.Global
transfGlobal (Surface.Def name args retty body) =
  Intermediate.DefFunction name args' retty' <$> transfBody args' retty' body
  where
    args' = map (second transfType) args
    retty' = transfType retty


-- | transform a program in Surface AST to Intermediate AST
transfProgram :: Surface.Program -> Transform Intermediate.Program
transfProgram (Surface.Program glbls) =
  addGlobalCtx ctx $ do
    glbls' <- traverse transfGlobal glbls
    pure $! Intermediate.Program glbls'
  where
    ctx = map (Surface.globalName &&& transfGlobalType) glbls
    transfGlobalType (Surface.Def _ args' retty _) =
      Intermediate.TFunction (map (transfType . snd) args') (transfType retty)

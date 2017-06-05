{-# LANGUAGE TupleSections #-}

module Simply.Intermediate.FromSurface
  ( fromSurface
  ) where

import Protolude hiding (Type)

import Control.Monad (zipWithM)
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

-- | A variable differentiating on its scope of origin
data ScopedVar
  = LocalVar  Intermediate.Type
  | GlobalVar Intermediate.Type
  deriving (Show, Eq, Ord)

-- | the context maps names to types
type Context = Map Name ScopedVar


-- | A transform reads the global and local context,
-- writes new global definitions,
-- and keeps a counter (state) for fresh variable names.
--
-- Transform a = Context -> Context -> Count -> (Count, Global, a)
type Transform = RWS Context (DList Intermediate.Global) Int


----------------------------------------------------------------------
-- Interface

-- | Transform Surface AST to Intermediate AST.
--
-- Every type-correct "Simply" program can be transformed.
--
-- It is illegal to pass a program that is not type-correct.
fromSurface :: Surface.Program -> Intermediate.Program
fromSurface program = Intermediate.Program (DList.apply extra globals)
  where
    (Intermediate.Program globals, extra) =
      evalRWS (transfProgram program) Map.empty 0


----------------------------------------------------------------------
-- Context Handling

with :: [Intermediate.Arg] -> Transform a -> Transform a
with vars = local . Map.union . Map.fromList $
  [ (name, LocalVar type_) | (name, type_) <- vars ]

lookup :: Name -> Transform ScopedVar
lookup name = do
  mbVar <- asks $ Map.lookup name
  case mbVar of
    Just var -> pure var
    Nothing  -> panic $
      "[Simply.Intermediate.FromSurface.lookup] \
      \Undefined variable: " <> show name


----------------------------------------------------------------------
-- State Handling

-- | produce a fresh name with the given prefix and bump the counter
freshname :: Text -> Transform Text
freshname pref = do
  n <- get
  modify (+1)
  pure $! pref <> show n


-- | add a global closure definition
defclosure
  :: Name
  -> [Intermediate.Arg]
  -> [Intermediate.Arg]
  -> Intermediate.Type
  -> Intermediate.Expr
  -> Transform ()
defclosure name env args retty body =
  tell $ DList.singleton (Intermediate.DefClosure name env args retty body)


-- | Lift an expression that expects the given arguments
-- and produces the given result type to a closure.
-- Capture all free local variables in the expression.
liftClosure
  :: [Intermediate.Arg]
  -> Intermediate.Type
  -> Intermediate.Expr
  -> Transform Intermediate.Expr
liftClosure arglist retty body = do
  body' <- convert retty body
  name <- freshname "__closure_"
  let
    env = Map.toList $ Intermediate.freeVars body' Map.\\ Map.fromList arglist
    capture = map (uncurry $ flip Intermediate.Var) env
  defclosure name env arglist retty body'
  pure $! Intermediate.Closure (Intermediate.TClosure (map snd arglist) retty) name capture


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
commonType :: Intermediate.Type -> Intermediate.Type -> Intermediate.Type

commonType (Intermediate.TFunction [] a) b | a == b = a
commonType (Intermediate.TClosure  [] a) b | a == b = a
commonType a (Intermediate.TFunction [] b) | a == b = a
commonType a (Intermediate.TClosure  [] b) | a == b = a

commonType (Intermediate.TFunction [] a) (Intermediate.TFunction [] b) | a == b = a
commonType (Intermediate.TFunction [] a) (Intermediate.TClosure  [] b) | a == b = a
commonType (Intermediate.TClosure  [] a) (Intermediate.TFunction [] b) | a == b = a
commonType (Intermediate.TClosure  [] a) (Intermediate.TClosure  [] b) | a == b = a

commonType (Intermediate.TFunction args1 ret1) (Intermediate.TClosure args2 ret2)
  | args1 == args2 && ret1 == ret2
  = Intermediate.TClosure args1 ret1
commonType (Intermediate.TClosure args1 ret1) (Intermediate.TFunction args2 ret2)
  | args1 == args2 && ret1 == ret2
  = Intermediate.TClosure args1 ret1

commonType a b | a == b = a

commonType a b
  | (args1, ret1) <- Intermediate.unfoldFunctionType a
  , (args2, ret2) <- Intermediate.unfoldFunctionType b
  , args1 == args2 && ret1 == ret2
  = Intermediate.TClosure args1 ret1

commonType a b = panic $
  "[Simply.Intermediate.FromSurface.commonType] \
  \No common type between " <> show a <> " and " <> show b


-- | Convert (if necessary) two expressions to their common type.
--
-- See 'promoteType'.
--
-- The function is partial and fails if there is no common type.
promote :: Intermediate.Expr -> Intermediate.Expr -> Transform (Intermediate.Expr, Intermediate.Expr)
promote a b = (,) <$> convert ty a <*> convert ty b
  where ty = Intermediate.exprType a `commonType` Intermediate.exprType b


-- | Convert an expression to the given type.
--
-- The function is partial and fails if the conversion is illegal.
convert :: Intermediate.Type -> Intermediate.Expr -> Transform Intermediate.Expr
convert as expr =
  let ty = Intermediate.exprType expr in
  case as of

    _ | as == ty -> pure expr

      | not (Intermediate.isFunctionType as)
      , Intermediate.isFunctionType ty
      , as == Intermediate.returnType ty
      , null (Intermediate.argTypes ty)
      -> call expr []

    Intermediate.TClosure args1 ret1
      | Intermediate.isFunctionType ty
      , args2 <- Intermediate.argTypes ty
      , ret2 <- Intermediate.returnType ty
      , args1 == args2 && ret1 == ret2
      -> partialCall expr []

    Intermediate.TClosure [] ret
      | ret == ty
      -> liftClosure [] ret expr

    Intermediate.TClosure args1 ret1
      | Intermediate.isFunctionType ty
      , (args2, ret2) <- Intermediate.unfoldFunctionType ty
      , args1 == args2 && ret1 == ret2
      -> do
        argnames <- traverse (const $ freshname "__arg_") args1
        let arglist = zip argnames args1
            passed = zipWith Intermediate.Var args1 argnames
        body <- call expr passed
        liftClosure arglist ret1 body

    _ -> panic $
      "[Simply.Intermediate.FromSurface.convert] \
      \can't convert from " <> show ty <> " to " <> show as


-- | transform a Surface AST expression to an Intermediate AST expression
--
-- The function is partial and fails on ill-formed expressions.
transfExpr :: Surface.Expr -> Transform Intermediate.Expr
transfExpr = \case

  Surface.Lit l@(LInt _) -> pure $! Intermediate.Lit Intermediate.TInt l
  Surface.Lit l@(LBool _) -> pure $! Intermediate.Lit Intermediate.TBool l

  Surface.Var name -> do
    var <- lookup name
    case var of
      LocalVar  type_ -> pure $ Intermediate.Var type_ name
      GlobalVar type_ -> pure $ Intermediate.Global type_ name

  Surface.Let name e ein -> do
    e' <- transfExpr e
    let ty = Intermediate.exprType e'
    ein' <- with [(name, ty)] $ transfExpr ein
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
    let (f, args) = Surface.unfoldApp app in
    join $ call <$> transfExpr f <*> traverse transfExpr args

  Surface.Lam arglist body -> do
    let arglist' = [ (n, transfType t) | (n, t) <- arglist ]
    body' <- with arglist' $ transfExpr body
    let ret' =
          case Intermediate.exprType body' of
            Intermediate.TFunction [] ret -> ret
            Intermediate.TFunction args ret -> Intermediate.TClosure args ret
            x -> x
    liftClosure arglist' ret' body'


-- | Construct a call
--
-- Must either be a variable referring to
-- a global function, a local function, or a closure;
-- or a closure expression.
--
-- The function is partial and fails if its pre-condition is violated.
call :: Intermediate.Expr -> [Intermediate.Expr] -> Transform Intermediate.Expr
call f args =
  case compare numargs numexpected of
    EQ -> saturatedCall f args
    LT -> partialCall f args
    GT -> do
      let (args1, args2) = splitAt numexpected args
      f' <- saturatedCall f args1
      call f' args2
  where
    numargs = length args
    numexpected = length . Intermediate.argTypes . Intermediate.exprType $ f

saturatedCall :: Intermediate.Expr -> [Intermediate.Expr] -> Transform Intermediate.Expr
saturatedCall f args = Intermediate.Call retty f <$> zipWithM convert argtys args
  where
    retty = Intermediate.returnType . Intermediate.exprType $ f
    argtys = Intermediate.argTypes . Intermediate.exprType $ f

partialCall :: Intermediate.Expr -> [Intermediate.Expr] -> Transform Intermediate.Expr
partialCall f given = do
  let missing = drop (length given) . Intermediate.argTypes . Intermediate.exprType $ f
  arglist <- forM missing $ \ t -> do
    n <- freshname "__arg_"
    pure (n, t)
  let extra = [ Intermediate.Var t n | (n, t) <- arglist ]
  body <- saturatedCall f (given ++ extra)
  let retty = Intermediate.returnType . Intermediate.exprType $ f
  liftClosure arglist retty body


-- | transform a global binding in "Simply" to IR
transfGlobal :: Surface.Global -> Transform Intermediate.Global
transfGlobal (Surface.Def name arglist retty body) = do
  let
    arglist' = map (second (transfType)) arglist
    retty' = transfType $ retty
  body' <- with arglist' $ convert retty' =<< transfExpr body
  pure $! Intermediate.DefFunction name arglist' retty' body'


transfProgram :: Surface.Program -> Transform Intermediate.Program
transfProgram (Surface.Program globals) = withGlobalContext $
  Intermediate.Program <$> traverse transfGlobal globals
  where
    withGlobalContext = local . const $ Map.fromList
      [ (name, GlobalVar ty)
      | Surface.Def name arglist retty _body <- globals
      , let ty = Intermediate.TFunction [ transfType t | (_, t) <- arglist ] (transfType retty)
      ]

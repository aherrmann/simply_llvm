module Simply.Surface.Gen.WellTyped
  ( genProgram
  ) where

import Protolude hiding (Type)

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Simply.Surface.AST


type Context = Map Name Type

with :: Context -> [Arg] -> Context
with context arglist = Map.fromList arglist `Map.union` context


genProgram :: Monad m => Gen m Program
genProgram = Gen.shrink shrinkProgram $ do
  globalSignatures <- genGlobalSignatures
  let context = typeFromSignature <$> Map.fromList globalSignatures
  globals <- for globalSignatures $ \ (name, (arglist, retty)) ->
    genGlobal name arglist retty context
  pure $! Program globals

  where

    genGlobalSignatures = do
      main <- genMainSignature
      globals <- Gen.map (Range.linear 0 10) genSignature
      Gen.shuffle (main : Map.toList globals)

    genMainSignature = do
      numargs <- Gen.int (Range.linear 0 10)
      arglist <- genArglistFor (replicate numargs TInt)
      pure $! ("main", (arglist, TInt))

    genSignature = do
      name <- genName
      argtys <- Gen.list (Range.linear 0 10) genFreshType
      arglist <- genArglistFor argtys
      retty <- genFreshType
      pure $! (name, (arglist, retty))

    typeFromSignature (arglist, retty) =
      foldr TArr retty [ t | (_, t) <- arglist ]

genGlobal :: Monad m => Name -> [Arg] -> Type -> Context -> Gen m Global
genGlobal name arglist retty context = do
  body <- genExpr retty (context `with` arglist)
  pure $! Def name arglist retty body

genExpr :: Monad m => Type -> Context -> Gen m Expr
genExpr ty context = Gen.shrink shrinkExpr $
  Gen.recursive Gen.choice final recursive
  where
    final = catMaybes [ Just (genConst ty), mbGenVar ty context ]
    recursive = catMaybes
      [ Just $ genLet ty context
      , Just $ genIf ty context
      , mbGenBinaryOp ty context
      , Just $ genApp ty context
      , mbGenLam ty context
      ]

genConst :: Monad m => Type -> Gen m Expr
genConst TInt = Lit . LInt <$> Gen.int (Range.linear 0 maxBound)
genConst TBool = Lit . LBool <$> Gen.bool
genConst tarr = do
  let
    (argtys, retty) = unfoldTArr tarr
    numargs = length argtys
  argnames <- Set.toList <$> Gen.set (Range.singleton numargs) genName
  let arglist = zip argnames argtys
  body <- genConst retty
  pure $! Lam arglist body

mbGenVar :: Monad m => Type -> Context -> Maybe (Gen m Expr)
mbGenVar ty context
  | null names = Nothing
  | otherwise  = Just (Var <$> Gen.element names)
  where
    names = Map.keys $ Map.filter (==ty) context

genLet :: Monad m => Type -> Context -> Gen m Expr
genLet ty context = do
  name <- genName
  boundty <- genType context
  bound <- genExpr boundty context
  body <- genExpr ty (context `with` [(name, boundty)])
  pure $! Let name bound body

genIf :: Monad m => Type -> Context -> Gen m Expr
genIf ty context = do
  cond <- genExpr TBool context
  then_ <- genExpr ty context
  else_ <- genExpr ty context
  pure $! If cond then_ else_

mbGenBinaryOp :: Monad m => Type -> Context -> Maybe (Gen m Expr)
mbGenBinaryOp TBool context =
  Just (BinaryOp Eql <$> genExpr TInt context <*> genExpr TInt context)
mbGenBinaryOp TInt context = Just $ do
  op <- BinaryOp <$> Gen.element [ Add, Sub, Mul ]
  Gen.subterm2 (genExpr TInt context) (genExpr TInt context) op
mbGenBinaryOp _ _ = Nothing

genApp :: Monad m => Type -> Context -> Gen m Expr
genApp ty context = do
  (fun, argtys) <- genFunctionReturning ty context
  args <- traverse (flip genExpr context) argtys
  pure $! List.foldl' App fun args

genFunctionReturning :: Monad m => Type -> Context -> Gen m (Expr, [Type])
genFunctionReturning ty context = Gen.choice $ catMaybes
  [ mbGenKnownFunctionReturning ty context
  , Just $ genFreshFunctionReturning ty context
  ]

genFreshFunctionReturning :: Monad m => Type -> Context -> Gen m (Expr, [Type])
genFreshFunctionReturning ty context = do
  argtys <- Gen.list (Range.linear 1 5) (genType context)
  let funty = List.foldr TArr ty argtys
  fun <- genExpr funty context
  pure $! (fun, argtys)

mbGenKnownFunctionReturning :: Monad m => Type -> Context -> Maybe (Gen m (Expr, [Type]))
mbGenKnownFunctionReturning ty context
  | null functions = Nothing
  | otherwise = Just (Gen.element functions)
  where
    functions = Map.toList $ Map.mapKeys Var $ Map.mapMaybe (makes ty) context
    makes want have
      | wants `List.isSuffixOf` haves, num > 0 = Just (take num haves)
      | otherwise = Nothing
      where
        wants = flatten want
        haves = flatten have
        num = length haves - length wants
    flatten TInt = [TInt]
    flatten TBool = [TBool]
    flatten (TArr a b) = a : flatten b

mbGenLam :: Monad m => Type -> Context -> Maybe (Gen m Expr)
mbGenLam ty context
  | null allArgtys = Nothing
  | otherwise = Just $ do
      numargs <- Gen.int (Range.linear 1 (length allArgtys))
      let
        (argtys, remaining) = splitAt numargs allArgtys
        bodyty = List.foldr TArr retty remaining
      arglist <- genArglistFor argtys
      body <- genExpr bodyty (context `with` arglist)
      pure $! Lam arglist body
  where
    (allArgtys, retty) = unfoldTArr ty

genName :: Monad m => Gen m Name
genName =
  Gen.filter (not . (=="main"))
  $ Text.cons <$> start <*> rest
  where
    start = Gen.lower
    rest  = Gen.text (Range.linear 0 5) (Gen.element chars)
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_', '\'']

genFreshType :: Monad m => Gen m Type
genFreshType =
  Gen.recursive Gen.choice
    [ pure TInt, pure TBool ]
    [ TArr <$> genFreshType <*> genFreshType ]

mbGenKnownType :: Monad m => Context -> Maybe (Gen m Type)
mbGenKnownType context
  | null types = Nothing
  | otherwise  = Just (Gen.element types)
  where
    types = Map.elems context

genType :: Monad m => Context -> Gen m Type
genType context =
  Gen.recursive Gen.choice
    (catMaybes [ mbGenKnownType context, Just genFreshType ])
    [ TArr <$> genType context <*> genType context ]

genArglistFor :: Monad m => [Type] -> Gen m [Arg]
genArglistFor argtys = do
  uniqueNames <- Gen.set (Range.singleton (length argtys)) genName
  argnames <- Gen.shuffle $ Set.toList uniqueNames
  pure $! zip argnames argtys

shrinkExpr :: Expr -> [Expr]
shrinkExpr expr = shrinkExprIf expr ++ catMaybes [ step expr ]

shrinkExprIf :: Expr -> [Expr]
shrinkExprIf = \case
  If _ then_ else_ -> [ then_, else_ ]
  _ -> []

shrinkProgram :: Program -> [Program]
shrinkProgram (Program globals) = do
  (fun, globals') <- picks globals
  let Def name arglist _retty body = fun
  guard (name /= "main")
  guard (name `Set.notMember` freeVars body)
  let
    subst
      | null arglist = body
      | otherwise    = Lam arglist body
    globals'' = map (substituteDef (name, subst)) globals'
  pure $! Program globals''
  where
    picks [] = []
    picks (x:xs) = [(x, xs)] ++ [(y, x:ys) | (y, ys) <- picks xs]
    substituteDef subst (Def defname arglist retty body)
      = Def defname arglist retty (substitute subst body)

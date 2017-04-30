module Simply.Surface.Gen.IllTyped
  ( genProgram
  ) where

import Protolude hiding (Type)

import qualified Data.Set as Set
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Simply.Surface.AST
import Simply.Surface.Parse (keywords)


genProgram :: Monad m => Gen m Program
genProgram = do
  globals <- Gen.list (Range.linear 0 10) genGlobal
  pure $! Program globals

genGlobal :: Monad m => Gen m Global
genGlobal = do
  name <- genName
  arglist <- Gen.list (Range.linear 0 5) genArg
  retty <- genType
  body <- genExpr
  pure $! Def name arglist retty body

genName :: Monad m => Gen m Name
genName =
  Gen.filter validName
  $ Text.cons <$> start <*> rest
  where
    validName n = n `Set.notMember` keywords
    start = Gen.lower
    rest  = Gen.text (Range.linear 0 5) (Gen.element chars)
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_', '\'']

genArg :: Monad m => Gen m Arg
genArg = do
  name <- genName
  type_ <- genType
  pure (name, type_)

genType :: Monad m => Gen m Type
genType =
  Gen.recursive Gen.choice
    [ pure TInt, pure TBool ]
    [ genType ]

genExpr :: Monad m => Gen m Expr
genExpr = Gen.shrink shrinkExpr $
  Gen.recursive Gen.choice
    [ Lit . LBool <$> Gen.bool
    , Lit . LInt <$> Gen.int (Range.linear 0 maxBound)
    , Var <$> genName
    ]
    [ genLet
    , genIf
    , genBinaryOp
    , genApp
    , genLam
    ]

shrinkExpr :: Expr -> [Expr]
shrinkExpr = \case
  Lit {} -> []
  Var {} -> []
  Let _ bound body -> [bound, body]
  If cond then_ else_ -> [cond, then_, else_]
  BinaryOp {} -> [] -- already covered by Gen.subterm2
  App f x -> [f, x]
  Lam _ body -> [body]

genLet :: Monad m => Gen m Expr
genLet = do
  name <- genName
  bound <- genExpr
  body <- genExpr
  pure $! Let name bound body

genIf :: Monad m => Gen m Expr
genIf = do
  cond <- genExpr
  then_ <- genExpr
  else_ <- genExpr
  pure $! If cond then_ else_

genBinaryOp :: Monad m => Gen m Expr
genBinaryOp = do
  op <- Gen.element [ Add, Sub, Mul, Eql ]
  Gen.subterm2 genExpr genExpr (BinaryOp op)

genApp :: Monad m => Gen m Expr
genApp = do
  f <- genExpr
  x <- genExpr
  pure $! App f x

genLam :: Monad m => Gen m Expr
genLam = do
  arglist <- Gen.list (Range.linear 1 5) genArg
  body <- genExpr
  pure $! Lam arglist body

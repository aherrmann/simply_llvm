{-# LANGUAGE DeriveGeneric #-}

module Simply.Intermediate.AST
  (
    -- * AST
    Name
  , Type (..)
  , Lit (..)
  , BinaryOp (..)
  , Arg
  , Expr (..)
  , Global (..)
  , Program (..)

    -- * Accessors
  , exprType
  , freeVars

    -- * Helpers
  , numArgs
  , argTypes
  , returnType
  , isFunctionType
  , unfoldFunctionType
  ) where

import Protolude hiding (Type)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.GenericPretty (Out)

import Simply.Orphans ()
import Simply.Surface.AST (Name, Lit (..), BinaryOp (..))


----------------------------------------------------------------------
-- AST

-- Take Name from Simply.


data Type
  = TInt
    -- ^ integer type
  | TBool
    -- ^ boolean type
  | TFunction [Type] Type
    -- ^ type of a global function
  | TClosure [Type] Type
    -- ^ type of a closure
  deriving (Show, Eq, Ord, Generic)


-- Take Lit from Simply.

-- Take Prim from Simply.


-- | argument in an argument list
--
-- Arguments are explicitly type annotated.
type Arg = (Name, Type)


-- | abstract syntax tree of expressions
data Expr
  = Lit Type Lit
    -- ^ literals
  | Var Type Name
    -- ^ local variables
  | Global Type Name
    -- ^ global variables
  | Let Type Name Expr Expr
    -- ^ let-bindings
  | If Type Expr Expr Expr
    -- ^ conditional branches
  | BinaryOp Type BinaryOp Expr Expr
    -- ^ primitive operations (fully applied)
  | Call Type Expr [Expr]
    -- ^ fully applied call to function or closure
  | Closure Type Name [Expr]
    -- ^ build a closure
  deriving (Show, Eq, Ord, Generic)


-- | global defintions
data Global
  = DefFunction Name [Arg] Type Expr
    -- ^ global function definition
  | DefClosure Name [Arg] [Arg] Type Expr
    -- ^ global closure-function definition,
    -- expects a capture and arguments
  deriving (Show, Eq, Ord, Generic)


-- | the program
data Program
  = Program [Global]
    -- ^ A program is a list of global definitions and a main function.
    -- We specify the return type explicitly.
  deriving (Show, Eq, Ord, Generic)


----------------------------------------------------------------------
-- Instances

instance Out Type
instance Out Expr
instance Out Global
instance Out Program


----------------------------------------------------------------------
-- Accessors

-- | extract the type of an expression
exprType :: Expr -> Type
exprType = \case
  Lit ty _ -> ty
  Var ty _ -> ty
  Global ty _ -> ty
  Let ty _ _ _ -> ty
  If ty _ _ _ -> ty
  BinaryOp ty _ _ _ -> ty
  Call ty _ _ -> ty
  Closure ty _ _ -> ty


-- | all unbound variables in an expression
-- (global variables are considered bound)
freeVars :: Expr -> Map Name Type
freeVars = \case
  Lit _ _ -> Map.empty
  Var ty name -> Map.singleton name ty
  Global _ _ -> Map.empty
  Let _ name ebound ein ->
    freeVars ebound `Map.union` Map.delete name (freeVars ein)
  If _ cond th el ->
    freeVars cond `Map.union` freeVars th `Map.union` freeVars el
  BinaryOp _ _ a b -> freeVars a `Map.union` freeVars b
  Call _ fun args -> Map.unions (map freeVars (fun:args))
  Closure _ _ env -> Map.unions (map freeVars env)


----------------------------------------------------------------------
-- Helpers

-- | number of expected arguments due to a function's type
numArgs :: Type -> Int
numArgs = length . argTypes

-- | argument types due to a function's type
--   values count as functions w/o arguments
argTypes :: Type -> [Type]
argTypes = \case
  TInt -> []
  TBool -> []
  TFunction args _ -> args
  TClosure args _ -> args

-- | a function's return type due to its type
--   values count as functions w/o arguments
returnType :: Type -> Type
returnType = \case
  TInt -> TInt
  TBool -> TBool
  TFunction _ retty -> retty
  TClosure _ retty -> retty

-- | distinguish functions and values
isFunctionType :: Type -> Bool
isFunctionType = \case
  TInt -> False
  TBool -> False
  TFunction _ _ -> True
  TClosure _ _ -> True

unfoldFunctionType :: Type -> ([Type], Type)
unfoldFunctionType = \case
  TInt -> ([], TInt)
  TBool -> ([], TBool)
  TFunction args ret ->
    let (args', ret') = unfoldFunctionType ret in
    (args ++ args', ret')
  TClosure args ret ->
    let (args', ret') = unfoldFunctionType ret in
    (args ++ args', ret')

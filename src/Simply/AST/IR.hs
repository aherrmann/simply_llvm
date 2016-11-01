{-# LANGUAGE DeriveGeneric #-}

module Simply.AST.IR
  (
    -- * AST
    Name
  , Type (..)
  , Lit (..)
  , Prim (..)
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
  ) where

import Protolude
import Control.Arrow ((&&&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.GenericPretty (Out)
import Simply.AST.Simply (Name, Lit (..), Prim (..))
import Simply.Orphans


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
  | LVar Type Name
    -- ^ local variables
  | GVar Type Name
    -- ^ global variables
  | Let Type Name Expr Expr
    -- ^ let-bindings
  | If Type Expr Expr Expr
    -- ^ conditional branches
  | Prim Type Prim [Expr]
    -- ^ primitive operations (fully applied)
  | CallFunction Type Name [Expr]
    -- ^ function calls (fully applied)

  | MakeClosure Type Name [Expr]
    -- ^ build a closure, name and capture
  | CallClosure Type Expr [Expr]
    -- ^ call a closure (fully applied)
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
  = Program [Global] [Arg] Type Expr
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
  LVar ty _ -> ty
  GVar ty _ -> ty
  Let ty _ _ _ -> ty
  If ty _ _ _ -> ty
  Prim ty _ _ -> ty
  CallFunction ty _ _ -> ty
  MakeClosure ty _ _ -> ty
  CallClosure ty _ _ -> ty


-- | all unbound variables in an expression
-- (global variables are considered bound)
freeVars :: Expr -> Map Name Type
freeVars = \case
  Lit _ _ -> Map.empty
  LVar ty name -> Map.singleton name ty
  GVar _ _ -> Map.empty
  Let _ name ebound ein ->
      freeVars ebound `Map.union` Map.delete name (freeVars ein)
  If _ cond th el ->
      freeVars cond `Map.union` freeVars th `Map.union` freeVars el
  Prim _ _ args -> Map.unions (map freeVars args)
  CallFunction _ _ args -> Map.unions (map freeVars args)
  MakeClosure _ _ env -> Map.unions (map freeVars env)
  CallClosure _ fun args -> Map.unions (freeVars fun : map freeVars args)


----------------------------------------------------------------------
-- Helpers

-- | number of expected arguments due to a function's type
numArgs :: Type -> Int
numArgs = \case
  TInt -> 0
  TBool -> 0
  TFunction args _ -> length args
  TClosure args _ -> length args

-- | argument types due to a function's type
argTypes :: Type -> [Type]
argTypes = \case
  TInt -> []
  TBool -> []
  TFunction args _ -> args
  TClosure args _ -> args

-- | a function's return type due to its type
returnType :: Type -> Type
returnType = \case
  TFunction _ retty -> retty
  TClosure _ retty -> retty
  _ -> panic "Not a function type"

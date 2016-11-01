{-# LANGUAGE DeriveGeneric #-}

module Simply.AST.Simply
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

    -- * Smart Constructors
  , (->.)
  , int
  , true
  , false
  , add
  , sub
  , mul
  , eql
  , (+.)
  , (-.)
  , (*.)
  , (==.)
  , (@.)

    -- * Accessors
  , freeVars
  , globalName
  , globalType

    -- * Helpers
  , unfoldTArr
  , foldTArr
  , unfoldApp
  ) where

import Protolude
import Control.Arrow ((&&&))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Text.PrettyPrint.GenericPretty (Out)
import Simply.Orphans


----------------------------------------------------------------------
-- AST

type Name = Text


data Type
  = TInt
    -- ^ integer type
  | TBool
    -- ^ boolean type
  | TArr Type Type
    -- ^ function type
    --
    -- @
    -- (Int -> Bool) -> Int -> Int
    -- @
  deriving (Show, Eq, Ord, Generic)


-- | literal constants
data Lit
  = LInt Int
  -- ^ integer literals
  | LBool Bool
  -- ^ boolean literals
  deriving (Show, Eq, Ord, Generic)


-- | primitive operations
data Prim
  = Add
  -- ^ integer addition @Int -> Int -> Int@
  | Sub
  -- ^ integer subtraction @Int -> Int -> Int@
  | Mul
  -- ^ integer multiplication @Int -> Int -> Int@
  | Eql
  -- ^ integer equality comparison @Int -> Int -> Bool@
  deriving (Show, Eq, Ord, Generic)


-- | argument in an argument list
--
-- Arguments are explicitly type annotated.
type Arg = (Name, Type)


-- | abstract syntax tree of expressions
data Expr
  = Lit Lit
    -- ^ literals
  | Var Name
    -- ^ variables
  | Let Name Expr Expr
    -- ^ let-bindings
    --
    -- @
    -- let a = 2 + 4
    -- in  a * 3
    -- @
  | If Expr Expr Expr
    -- ^ conditional branches
    --
    -- @
    -- if (a == 3)
    --   then 42
    --   else 13
    -- @
  | Prim Prim
    -- ^ primitive operations
    --
    -- @
    -- a + b * 3 - 2 == 6
    -- @
  | App Expr Expr
    -- ^ function application
    --
    -- @
    -- f x
    -- @
  | Lam [Arg] Expr
    -- ^ lambda abstraction
    --
    -- @
    -- \\ (x : Int) (y : Int)
    --  -> x + y
    -- @
  deriving (Show, Eq, Ord, Generic)


-- | global defintions
data Global
  = Def Name [Arg] Type Expr
    -- ^ function definition
    --
    -- The result type is explicitly annotated.
    --
    -- @
    --     foo (a : Int) (b : Int) : Int = a + b
    -- @
  deriving (Show, Eq, Ord, Generic)


-- | the program
data Program
  = Program [Global] [Arg] Expr
    -- ^ a program is a list of global definitions and a main function
    --
    -- The main function's result type can be inferred and has to be @Int@.
    --
    -- @
    -- globals
    --     factorial' (acc : Int) (n : Int) : Int =
    --         if n == 0 then
    --             acc
    --         else
    --             factorial' (acc * n) (n - 1)
    --     factorial (n : Int) : Int =
    --         factorial' 1 n
    -- main (n : Int) = factorial n
    -- @
  deriving (Show, Eq, Ord, Generic)


----------------------------------------------------------------------
-- Instances

instance IsString Expr where
  fromString = Var . toS

instance Out Type
instance Out Expr
instance Out Lit
instance Out Prim
instance Out Global
instance Out Program


----------------------------------------------------------------------
-- Smart Constructors

-- | arrow type
(->.) :: Type -> Type -> Type
(->.) = TArr
infixr 0 ->.

-- | integer literal
int :: Int -> Expr
int = Lit . LInt

-- | boolean literal @True@
true :: Expr
true = Lit $ LBool True

-- | boolean literal @False@
false :: Expr
false = Lit $ LBool True

-- | primitive addition
add :: Expr
add = Prim Add

-- | primitive subtraction
sub :: Expr
sub = Prim Sub

-- | primitive multiplication
mul :: Expr
mul = Prim Mul

-- | primitive equality comparison
eql :: Expr
eql = Prim Eql

-- | primitive addition
(+.) :: Expr -> Expr -> Expr
(+.) a b = Prim Add @. [a, b]
infixl 6 +.

-- | primitive subtraction
(-.) :: Expr -> Expr -> Expr
(-.) a b = Prim Sub @. [a, b]
infixl 6 -.

-- | primitive multiplication
(*.) :: Expr -> Expr -> Expr
(*.) a b = Prim Mul @. [a, b]
infixl 7 *.

-- | primitive equality comparison
(==.) :: Expr -> Expr -> Expr
(==.) a b = Prim Eql @. [a, b]
infix 4 ==.

-- | function appliciation
(@.) :: Expr -> [Expr] -> Expr
(@.) = foldl' App
infixl 9 @.


----------------------------------------------------------------------
-- Accessors

-- | all unbound variables in an expression
-- (does not take global bindings into account)
freeVars :: Expr -> Set Name
freeVars = \case
    Lit _ -> Set.empty
    Var name -> Set.singleton name
    Let name ebound ein ->
        freeVars ebound `Set.union` Set.delete name (freeVars ein)
    If cond th el ->
        freeVars cond `Set.union` freeVars th `Set.union` freeVars el
    Prim _ -> Set.empty
    App fun arg ->
        freeVars fun `Set.union` freeVars arg
    Lam args body ->
        freeVars body Set.\\ Set.fromList (map fst args)

-- | name of a global binding
globalName :: Global -> Name
globalName (Def name _ _ _) = name

-- | type of a global binding
globalType :: Global -> Type
globalType (Def _ args retty _) = foldTArr (map snd args) retty


----------------------------------------------------------------------
-- Helpers

-- | extract argument list and return types from function type
--
-- @
-- unfoldTArr ((Int -> Bool) -> Int -> Bool)  =  ([Int -> Bool, Int], Bool)
-- @
unfoldTArr :: Type -> ([Type], Type)
unfoldTArr (TArr a b) =
    let (args, ret) = unfoldTArr b
    in  (a:args, ret)
unfoldTArr ty = ([], ty)

-- | inverse of 'unfoldTArr'
foldTArr :: [Type] -> Type -> Type
foldTArr = flip (foldr TArr)

-- | flatten a nested application
--
-- @
-- unfoldApp (((f a) b) c)  =  (f, [a, b, c])
-- @
unfoldApp :: Expr -> (Expr, [Expr])
unfoldApp = go []
  where
    go args (App fun arg) = go (arg:args) fun
    go args fun = (fun, args)

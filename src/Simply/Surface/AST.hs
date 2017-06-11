{-# LANGUAGE DeriveGeneric #-}

module Simply.Surface.AST
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
  , argNames
  , argTypes
  , freeVars
  , globalName
  , globalType

    -- * Helpers
  , unfoldTArr
  , flatTArr
  , foldTArr
  , appPrecedence
  , opPrecedence
  , unfoldApp
  , substitute
  , step
  , apply
  ) where

import Protolude hiding (Type)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)


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


-- | primitive binary operations
data BinaryOp
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
  | BinaryOp BinaryOp Expr Expr
    -- ^ primitive binary operations
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
newtype Program
  = Program [Global]
    -- ^ a program is a list of global definitions including the main function.
    --
    -- The main function's result type has to be @Int@.
    --
    -- @
    -- fun factorial' (acc : Int) (n : Int) : Int =
    --     if n == 0 then
    --         acc
    --     else
    --         factorial' (acc * n) (n - 1)
    -- fun factorial (n : Int) : Int =
    --     factorial' 1 n
    -- fun main (n : Int) : Int = factorial n
    -- @
  deriving (Show, Eq, Ord, Generic)


----------------------------------------------------------------------
-- Instances

instance IsString Expr where
  fromString = Var . toS


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
add = Lam [("a", TInt), ("b", TInt)] (BinaryOp Add (Var "a") (Var "b"))

-- | primitive subtraction
sub :: Expr
sub = Lam [("a", TInt), ("b", TInt)] (BinaryOp Sub (Var "a") (Var "b"))

-- | primitive multiplication
mul :: Expr
mul = Lam [("a", TInt), ("b", TInt)] (BinaryOp Mul (Var "a") (Var "b"))

-- | primitive equality comparison
eql :: Expr
eql = Lam [("a", TInt), ("b", TInt)] (BinaryOp Eql (Var "a") (Var "b"))

-- | primitive addition
(+.) :: Expr -> Expr -> Expr
(+.) = BinaryOp Add
infixl 6 +.

-- | primitive subtraction
(-.) :: Expr -> Expr -> Expr
(-.) = BinaryOp Sub
infixl 6 -.

-- | primitive multiplication
(*.) :: Expr -> Expr -> Expr
(*.) = BinaryOp Mul
infixl 7 *.

-- | primitive equality comparison
(==.) :: Expr -> Expr -> Expr
(==.) = BinaryOp Eql
infix 4 ==.

-- | function appliciation
(@.) :: Expr -> [Expr] -> Expr
(@.) = foldl' App
infixl 9 @.


----------------------------------------------------------------------
-- Accessors

argNames :: [Arg] -> [Name]
argNames = map fst


argTypes :: [Arg] -> [Type]
argTypes = map snd

-- | all unbound variables in an expression
-- (does not take global bindings into account)
freeVars :: Expr -> Set Name
freeVars = \case
  Lit _ -> Set.empty
  Var name -> Set.singleton name
  Let name ebound ein ->
    freeVars ebound `Set.union` Set.delete name (freeVars ein)
  If cond then_ else_ ->
    freeVars cond `Set.union` freeVars then_ `Set.union` freeVars else_
  BinaryOp _ a b -> freeVars a `Set.union` freeVars b
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
unfoldTArr (TArr a b) = (a:args, ret)
  where (args, ret) = unfoldTArr b
unfoldTArr ty = ([], ty)

flatTArr :: Type -> [Type]
flatTArr (TArr a b) = a : flatTArr b
flatTArr ty = [ty]

-- | inverse of 'unfoldTArr'
foldTArr :: [Type] -> Type -> Type
foldTArr = flip (foldr TArr)

appPrecedence :: Int
appPrecedence = 5

opPrecedence :: BinaryOp -> Int
opPrecedence = \case
  Mul -> 4
  Add -> 2
  Sub -> 2
  Eql -> 1

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

substitute :: (Name, Expr) -> Expr -> Expr
substitute (name, subst) = \case

  Lit lit -> Lit lit

  Var name'
    | name' == name -> subst
    | otherwise -> Var name'

  Let name' bound body ->
    let
      bound' = recurse bound
      body'
        | name' /= name = recurse body
        | otherwise = body
    in
    Let name' bound' body'

  If cond then_ else_ ->
    If (recurse cond) (recurse then_) (recurse else_)

  BinaryOp op a b ->
    BinaryOp op (recurse a) (recurse b)

  App f a ->
    App (recurse f) (recurse a)

  Lam arglist body
    | name `notElem` [ argname | (argname, _) <- arglist ]
    -> Lam arglist (recurse body)
    | otherwise
    -> Lam arglist body

  where
    recurse = substitute (name, subst)

step :: Expr -> Maybe Expr
step = \case
  Lit _ -> Nothing
  Var _ -> Nothing
  Let name bound body -> Just (substitute (name, bound) body)
  If (Lit (LBool True)) then_ _ -> Just then_
  If (Lit (LBool False)) _ else_ -> Just else_
  If cond then_ else_ -> do
    cond' <- step cond
    Just (If cond' then_ else_)
  BinaryOp op (Lit (LInt a)) (Lit (LInt b)) ->
    case op of
      Add -> Just $ Lit $ LInt $ a + b
      Sub -> Just $ Lit $ LInt $ a - b
      Mul -> Just $ Lit $ LInt $ a * b
      Eql -> Just $ Lit $ LBool $ a == b
  BinaryOp op a b ->
    flip (BinaryOp op) b <$> step a
    <|> BinaryOp op a <$> step b
  App f a ->
    flip App a <$> step f
    <|> App f <$> step a
    <|> apply f a
  Lam _ _ -> Nothing

apply :: Expr -> Expr -> Maybe Expr
apply (Lam ((name, _):arglist) body) arg
  | null arglist = Just body'
  | otherwise    = Just $ Lam arglist body'
  where
    body' = substitute (name, arg) body
apply _ _ = Nothing

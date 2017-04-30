{-# LANGUAGE DeriveGeneric #-}

module Simply.Surface.TypeCheck
  (
    -- * Error Handling
    ErrorCode (..)
  , ExprError (..)
  , GlobalError (..)
  , ProgramError (..)

    -- * Type Checker
  , Context
  , checkExpr
  , checkGlobal
  , checkProgram

    -- * IO Interface
  , typeCheck
  ) where

import Protolude hiding (Type)

import Control.Arrow ((&&&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Text.Show.Prettyprint (prettyPrint)

import Simply.Surface.AST


----------------------------------------------------------------------
-- Error Handling

-- | Specific errors that occur during type-checking
data ErrorCode
  = UndefinedReference Name
    -- ^ an undefined variable was referenced
  | EmptyName
    -- ^ a variable name may not be empty
  | WrongType Type Type
    -- ^ mismatch between /expected/ and /actual/ type
  | TypeMismatch Type Type
    -- ^ mismatch between two types that should be equal
  | NotAFunction Type
    -- ^ expected a function but got something else
  | RecurringArgumentName [Arg]
    -- ^ an argument name appears more than once in an argument list
  | RecurringGlobalNames [Name]
    -- ^ these global names appear more than once
  | IllegalMainType Type
    -- ^ the main function must be a function from zero or more Int to Int
  | NoMainFunction
    -- ^ a program must have exactly one main function
  deriving (Show, Eq, Ord, Generic, Typeable)


-- | A type error within an expression
data ExprError
  = ExprError ErrorCode [Expr]
    -- ^ it carries an error code
    -- and a trace of expressions
    -- in which the error occurred (outside in)
  deriving (Show, Eq, Ord, Generic, Typeable)


-- | Extend the trace in an error by the given expression.
-- Identity on success.
exprErrorTrace :: Expr -> Either ExprError a -> Either ExprError a
exprErrorTrace expr = \case
  Left (ExprError code exprs) -> Left (ExprError code (expr:exprs))
  Right a -> Right a


-- | Packs an error code into an expression error with a singleton trace.
throwExprError :: Expr -> ErrorCode -> Either ExprError a
throwExprError expr code = Left $! ExprError code [expr]


-- | Packs an error code into an expression error with an empty trace.
throwExprError_ :: ErrorCode -> Either ExprError a
throwExprError_ code = Left $! ExprError code []


-- | A type error within a global binding
data GlobalError
  = GlobalError Name ErrorCode
    -- ^ an error directly in the global binding (not in its body)
  | GlobalBodyError Name ExprError
    -- ^ it carries the name of the global binding
    -- and the error that occurred in the body of the binding
  deriving (Show, Eq, Ord, Generic, Typeable)


-- | Forward an expression error
-- in the body of a global binding
-- as a global error
catchGlobalBodyError :: Name -> Either ExprError a -> Either GlobalError a
catchGlobalBodyError name = \case
  Left err -> Left $! GlobalBodyError name err
  Right a -> Right a


-- | A type error with the program
data ProgramError
  = ProgramError ErrorCode
    -- ^ an error directly in the program definition
    -- (not a global binding)
  | ProgramGlobalError GlobalError
    -- ^ an error with a global binding
  deriving (Show, Eq, Ord, Generic, Typeable)


-- | Forward a global error
-- in a global binding of the program
-- as a program error
catchProgramGlobalError :: Either GlobalError a -> Either ProgramError a
catchProgramGlobalError = \case
  Left err -> Left $! ProgramGlobalError err
  Right a -> Right a


----------------------------------------------------------------------
-- Type Checker


type Context = Map Name Type


checkExpr :: Context -> Expr -> Either ExprError Type
checkExpr ctx expr = exprErrorTrace expr $ case expr of

  Lit (LInt _) -> pure TInt
  Lit (LBool _) -> pure TBool

  Var name ->
    case Map.lookup name ctx of
      Nothing -> throwExprError_ $! UndefinedReference name
      Just ty -> pure ty

  Let name e1 e2 -> do
    unless (Text.length name > 0) $ throwExprError_ EmptyName
    ty1 <- checkExpr ctx e1
    ty2 <- checkExpr (Map.insert name ty1 ctx) e2
    pure ty2

  If cond th el -> do
    tyCond <- checkExpr ctx cond
    unless (tyCond == TBool) $
      throwExprError cond (WrongType TBool tyCond)
    tyTh <- checkExpr ctx th
    tyEl <- checkExpr ctx el
    unless (tyTh == tyEl) $ throwExprError_ (TypeMismatch tyTh tyEl)
    pure tyTh

  BinaryOp op a b -> do
    tyA <- checkExpr ctx a
    unless (tyA == TInt) $
      throwExprError a (WrongType TInt tyA)
    tyB <- checkExpr ctx b
    unless (tyB == TInt) $
      throwExprError b (WrongType TInt tyB)
    case op of
      Add -> pure TInt
      Sub -> pure TInt
      Mul -> pure TInt
      Eql -> pure TBool

  App f x -> do
    fty <- checkExpr ctx f
    xty <- checkExpr ctx x
    case fty of
      TArr argty retty -> do
        unless (xty == argty) $ throwExprError_ (TypeMismatch argty xty)
        pure retty
      _ -> throwExprError f (NotAFunction fty)

  Lam args body -> do
    forM_ args $ \(name, _) ->
      unless (Text.length name > 0) $ throwExprError_ EmptyName
    let argCtx = Map.fromList args
    unless (Map.size argCtx == length args) $
      throwExprError_ (RecurringArgumentName args)
    retty <- checkExpr (argCtx `Map.union` ctx) body
    pure $! foldTArr (map snd args) retty


checkGlobal :: Context -> Global -> Either GlobalError ()
checkGlobal ctx (Def name args retty body) = do
  unless (Text.length name > 0) $ Left $! GlobalError name EmptyName
  forM_ args $ \(argname, _) ->
    unless (Text.length argname > 0) $ Left $! GlobalError name EmptyName
  let argCtx = Map.fromList args
  bodyTy <- catchGlobalBodyError name $
    checkExpr (argCtx `Map.union` ctx) body
  unless (bodyTy == retty) $
    Left $! GlobalError name (TypeMismatch retty bodyTy)


checkProgram :: Program -> Either ProgramError ()
checkProgram (Program globals) = do
  unless (null multiNames) $
    Left $! ProgramError $ RecurringGlobalNames multiNames
  _ <- catchProgramGlobalError $ traverse (checkGlobal ctx) globals
  case Map.lookup "main" ctx of
    Nothing -> Left $! ProgramError NoMainFunction
    Just mainty
      | (args, ret) <- unfoldTArr mainty
      , all (==TInt) args && ret == TInt
      -> Right ()
      | otherwise
      -> Left $! ProgramError $ IllegalMainType mainty
  where
    ctx = Map.fromList $ map (globalName &&& globalType) globals
    multiNames = globals
      & map globalName
      & foldl' insert Map.empty
      & Map.filter (>1)
      & Map.keys
    insert m n = Map.insertWith (+) n (1::Int) m


----------------------------------------------------------------------
-- IO Interface


-- | type-check the program, print and abort if an error occurs
typeCheck :: Program -> IO Program
typeCheck prog =
  case checkProgram prog of
    Left err -> do
      prettyPrint err
      panic "Type-checking error"
    Right _ -> pure prog

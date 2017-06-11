{-# LANGUAGE DeriveGeneric #-}

module Simply.Surface.TypeCheck
  (
    -- * Convenient Interface
    typeCheck
  , typeCheck'

    -- * Type Checked Program
  , TypeSafeProgram
  , programMainNumArgs
  , programMainArgNames
  , programMainFunction
  , programGlobals

    -- * Error Handling
  , TypeCheckError (..)
  , ExprError (..)
  , GlobalError (..)
  , ProgramError (..)
  ) where

import Protolude hiding (Type, group)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Render.Terminal

import Simply.Surface.AST
import Simply.Surface.Parse (isKeyword)
import Simply.Surface.Pretty hiding (prettyPrint)


----------------------------------------------------------------------
-- Convenient Interface

typeCheck :: Program -> Either (Doc Highlight) TypeSafeProgram
typeCheck = first ppProgramError . checkProgram

-- | type-check the program, print and abort if an error occurs
typeCheck' :: Program -> IO TypeSafeProgram
typeCheck' = either printErrorAndDie pure . checkProgram
  where
    printErrorAndDie err = do
      Render.Terminal.renderIO stderr $
        layoutSmart defaultLayoutOptions $
        reAnnotate highlightTerminal $
        ppProgramError err
      exitFailure


----------------------------------------------------------------------
-- Type Checked Program

data TypeSafeProgram = TypeSafeProgram
  { programGlobals :: [Global]
  , programMainArgNames :: [Name]
  , programMainFunction :: Expr
  }
  deriving (Show, Eq, Ord, Generic)

programMainNumArgs :: TypeSafeProgram -> Int
programMainNumArgs = length . programMainArgNames


----------------------------------------------------------------------
-- Type Checker

type Context = Map Name Type

with :: Context -> [Arg] -> Context
with ctx arglist = Map.fromList arglist `Map.union` ctx

checkProgram :: Program -> Either ProgramError TypeSafeProgram
checkProgram (Program globals) = do
  -- check for recurring global names
  let nameMultiples = enlistMultiples (map globalName globals)
  unless (null nameMultiples) $
    throwProgramError $! RecurringGlobalNames nameMultiples
  -- check global definitions
  rethrowProgramGlobalError $ forM_ globals $ checkGlobal ctx
  let globals' = Map.fromList [ (globalName g, g) | g <- globals ]
  -- check main function
  main <- maybe (throwProgramError NoMainFunction) pure $
    Map.lookup "main" globals'
  let
    Def _ mainArglist mainRetty mainBody = main
    mainArgTypes = argTypes mainArglist
    mainArgNames = argNames mainArglist
  unless (all (== TInt) mainArgTypes && mainRetty == TInt) $
    throwProgramError $! IllegalMainFunction (foldTArr mainArgTypes mainRetty)
  -- Construct type checked program
  pure $! TypeSafeProgram
    { programGlobals = Map.elems $ Map.delete "main" globals'
    , programMainArgNames = mainArgNames
    , programMainFunction = mainBody
    }
  where
    ctx = Map.fromList $ map nameAndType globals
    nameAndType global = (globalName global, globalType global)

checkGlobal :: Context -> Global -> Either GlobalError ()
checkGlobal ctx (Def name arglist retty body) = do
  bodyty <- rethrowGlobalBodyError name $
    checkExpr (ctx `with` arglist) body
  unless (bodyty == retty) $
    throwError $! GlobalError name (ReturnTypeMismatch retty bodyty)

checkExpr :: Context -> Expr -> Either ExprError Type
checkExpr ctx expr = exprErrorTrace expr $ case expr of

  Lit (LInt _) -> pure TInt
  Lit (LBool _) -> pure TBool

  Var name -> do
    first exprError_ $ checkName name
    case Map.lookup name ctx of
      Nothing -> throwExprError_ $! VariableNotInScope name
      Just ty -> pure ty

  Let name bound body -> do
    boundty <- checkExpr ctx bound
    checkExpr (Map.insert name boundty ctx) body

  If cond then_ else_ -> do
    condty <- checkExpr ctx cond
    unless (condty == TBool) $
      throwExprError cond $! ExpectedButGot TBool condty
    thenty <- checkExpr ctx then_
    elsety <- checkExpr ctx else_
    unless (thenty == elsety) $ throwExprError_ (NotTheSame thenty elsety)
    pure thenty

  BinaryOp op a b -> do
    aty <- checkExpr ctx a
    unless (aty == TInt) $
      throwExprError a (ExpectedButGot TInt aty)
    bty <- checkExpr ctx b
    unless (bty == TInt) $
      throwExprError b (ExpectedButGot TInt bty)
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
        unless (xty == argty) $ throwExprError_ (ExpectedButGot argty xty)
        pure retty
      _ -> throwExprError f (NotAFunction fty)

  Lam arglist body -> do
    let argMultiples = enlistMultiples (argNames arglist)
    unless (null argMultiples) $
      throwExprError_ (RecurringArgumentNames argMultiples)
    retty <- checkExpr (ctx `with` arglist) body
    pure $! foldTArr (argTypes arglist) retty

checkName :: Name -> Either TypeCheckError ()
checkName name
  | Text.null name = throwError NameIsEmpty
  | isKeyword name = throwError $! NameIsKeyword name
  | otherwise = pure ()

countOccurrences :: Ord a => [a] -> Map a Int
countOccurrences = foldl' (flip insert) Map.empty
  where
    insert item = Map.insertWith (+) item 1

enlistMultiples :: Ord a => [a] -> [a]
enlistMultiples = Map.keys . Map.filter (> 1) . countOccurrences


----------------------------------------------------------------------
-- Error Handling

-- | A type error with the program
data ProgramError
  = ProgramError TypeCheckError
    -- ^ an error directly in the program definition
    -- (not a global binding)
  | ProgramGlobalError GlobalError
    -- ^ an error with a global binding
  deriving (Show, Eq, Ord, Generic, Typeable)

-- | A type error within a global binding
data GlobalError
  = GlobalError Name TypeCheckError
    -- ^ an error directly in the global binding (not in its body)
  | GlobalBodyError Name ExprError
    -- ^ it carries the name of the global binding
    -- and the error that occurred in the body of the binding
  deriving (Show, Eq, Ord, Generic, Typeable)

-- | A type error within an expression
data ExprError
  = ExprError TypeCheckError [Expr]
    -- ^ it carries an error code
    -- and a trace of expressions
    -- in which the error occurred (outside in)
  deriving (Show, Eq, Ord, Generic, Typeable)

-- | Specific errors that occur during type-checking
data TypeCheckError
  = NameIsEmpty
    -- ^ The empty string is not a valid name
  | NameIsKeyword Name
    -- ^ A keyword is not a valid name
  | VariableNotInScope Name
    -- ^ Reference to unknown variable
  | ExpectedButGot Type Type
    -- ^ Value of unexpected type encountered
  | NotTheSame Type Type
    -- ^ Two values should have the same type but don't
  | NotAFunction Type
    -- ^ Expected a function
  | RecurringArgumentNames [Name]
    -- ^ An argument name is used multiple times in one argument list
  | ReturnTypeMismatch Type Type
    -- ^ The return type does not match the body type
  | RecurringGlobalNames [Name]
    -- ^ Multiple global definitions share a name
  | IllegalMainFunction Type
    -- ^ The main function must be a function from integers to integer
  | NoMainFunction
    -- ^ A program has to have a main function
  deriving (Show, Eq, Ord, Generic, Typeable)


throwProgramError :: TypeCheckError -> Either ProgramError a
throwProgramError = first ProgramError . throwError

-- | Forward a global error
-- in a global binding of the program
-- as a program error
rethrowProgramGlobalError :: Either GlobalError a -> Either ProgramError a
rethrowProgramGlobalError = first ProgramGlobalError

-- | Forward an expression error
-- in the body of a global binding
-- as a global error
rethrowGlobalBodyError :: Name -> Either ExprError a -> Either GlobalError a
rethrowGlobalBodyError name = first (GlobalBodyError name)

exprError :: Expr -> TypeCheckError -> ExprError
exprError expr e = ExprError e [expr]

exprError_ :: TypeCheckError -> ExprError
exprError_ e = ExprError e []

-- | Extend the trace in an error by the given expression.
-- Identity on success.
exprErrorTrace :: Expr -> Either ExprError a -> Either ExprError a
exprErrorTrace expr = \case
  Left (ExprError code exprs) -> Left (ExprError code (expr:exprs))
  Right a -> Right a

-- | Packs an error code into an expression error with a singleton trace.
throwExprError :: Expr -> TypeCheckError -> Either ExprError a
throwExprError expr = first (exprError expr) . throwError

-- | Packs an error code into an expression error with an empty trace.
throwExprError_ :: TypeCheckError -> Either ExprError a
throwExprError_ = first exprError_ . throwError


ppProgramError :: ProgramError -> Doc Highlight
ppProgramError (ProgramError err) =
  ppErrMsg "Type-Checking Error" $
    ppTypeCheckError err
ppProgramError (ProgramGlobalError err) =
  ppErrMsg "Type-Checking Error" $
    ppGlobalError err

ppGlobalError :: GlobalError -> Doc Highlight
ppGlobalError (GlobalError name err) =
  ppErrMsg ("Error with" <+> dquote <> ppName name <> dquote) $
    ppTypeCheckError err
ppGlobalError (GlobalBodyError name err) =
  ppErrMsg ("Error in" <+> dquote <> ppName name <> dquote) $
    ppExprError err

ppExprError :: ExprError -> Doc Highlight
ppExprError (ExprError err []) = ppTypeCheckError err
ppExprError (ExprError err ctx) =
  let
    ppCtxErr ctxErr' =
      vsep
        [ "Encountered in"
        , indent 2 ctxErr'
        ]
    ctx' = vsep $ map (ppCtxErr . ppExpr) $ take 3 $ reverse ctx
  in
  vsep [ppTypeCheckError err, ctx']

ppTypeCheckError :: TypeCheckError -> Doc Highlight
ppTypeCheckError = \case
  NameIsEmpty -> ppErrMsg "Invalid name"
    "The empty string is not a valid name."
  NameIsKeyword name -> ppErrMsg "Invalid name"
    dquote <> pretty name <> dquote <+> "is a keyword."
  VariableNotInScope name -> ppErrMsg "Variable not in scope" $
    dquote <> pretty name <> dquote <+> "is not defined."
  ExpectedButGot expected got -> ppErrMsg "Unexpected type" $
    let
      expected' = nest 2 $ vsep [ "Expected", ppType expected ]
      got' = nest 2 $ vsep [ "but got", ppType got ]
    in
    group $ vsep [ expected', got' ]
  NotTheSame a b -> ppErrMsg "Type mismatch" $
    ppType a <+> "and" <+> ppType b <+> "are not the same type."
  NotAFunction ty -> ppErrMsg "Too many arguments" $
    ppType ty <+> "is not a function type."
  RecurringArgumentNames names -> ppErrMsg "Recurring argument names" $
    "The following names are used multiple times" <+> pretty names
  ReturnTypeMismatch expected got -> ppErrMsg "Mismatch in return type" $
    let
      expected' = nest 2 $ vsep [ "Expected", ppType expected ]
      got' = nest 2 $ vsep [ "but got", ppType got ]
    in
    group $ vsep [ expected', got' ]
  RecurringGlobalNames names -> ppErrMsg "Recurring global names" $
    "The following names are used multiple times" <+> pretty names
  IllegalMainFunction mainty -> ppErrMsg "Illegal main function" $
    "The main function must be a function from integers to integers. \
    \Instead it has the type" <+> ppType mainty
  NoMainFunction -> ppErrMsg "No main function"
    "A program has to have a main function."

ppErrMsg :: Doc Highlight -> Doc Highlight -> Doc Highlight
ppErrMsg heading err =
  let heading' = annotate HlError heading <> colon
  in
  group $
    vsep [ heading', indent 2 err ]
    `flatAlt`
    hsep [ heading', err ]

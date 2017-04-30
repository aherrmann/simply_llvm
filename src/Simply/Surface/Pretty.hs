module Simply.Surface.Pretty
  ( prettyPrint
  , prettyText
  , prettyPlainText
  , prettyPlainTextFast

  , ppProgram
  ) where

import Protolude hiding (Type, group)

import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Render.Text
import Data.Text.Prettyprint.Doc.Render.Terminal (Color(..), color, colorDull, bold)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Render.Terminal

import Simply.Surface.AST

prettyPrint :: Program -> IO ()
prettyPrint =
  Render.Terminal.putDoc
  . reAnnotate highlightTerminal
  . ppProgram

prettyText :: Program -> Text
prettyText =
  Render.Terminal.renderStrict
  . layoutSmart defaultLayoutOptions
  . reAnnotate highlightTerminal
  . ppProgram

prettyPlainText :: Program -> Text
prettyPlainText = Render.Text.renderStrict . layoutSmart defaultLayoutOptions . ppProgram

prettyPlainTextFast :: Program -> Text
prettyPlainTextFast = Render.Text.renderStrict . layoutCompact . ppProgram


data Highlight
  = HlName
  | HlKeyword
  | HlType
  | HlBinaryOp
  | HlSymbol
  | HlLiteral
  deriving (Eq, Show)


highlightTerminal :: Highlight -> Render.Terminal.AnsiStyle
highlightTerminal = \case
  HlName -> mempty
  HlKeyword -> colorDull Blue
  HlType -> color Red
  HlBinaryOp -> colorDull Blue
  HlSymbol -> bold <> colorDull Blue
  HlLiteral -> color Red


ppProgram :: Program -> Doc Highlight
ppProgram (Program globals) =
  concatWith
    (surround $ hardline <> hardline)
    (map ppGlobal globals)

ppGlobal :: Global -> Doc Highlight
ppGlobal (Def name arglist retty body) =
  let
    def' = ppKeyword "def"
    name' = ppName name
    arg'list = map ppArg arglist
    annot' = ppTypeAnnot retty
    equals' = ppEquals
    body' = ppExpr body

    defNameArgs'
      | [] <- arg'list
      = group $
          vsep [ def' <+> name', indent 2 annot' ]
          `flatAlt`
          hsep [ def', name', annot' ]
      | otherwise
      = group $
          vsep [ def' <+> name', indent 2 (vsep arg'list), indent 2 annot' ]
          `flatAlt`
          hsep [ def', name', hsep arg'list, annot' ]
    equalsBody' =
      group $
        vsep [ equals', indent 2 body' ]
        `flatAlt`
        hsep [ equals', body' ]
  in
  group $
    vsep [ defNameArgs', equalsBody' ]
    `flatAlt`
    hsep [ defNameArgs', equalsBody' ]

ppName :: Name -> Doc Highlight
ppName = annotate HlName . pretty

ppArg :: Arg -> Doc Highlight
ppArg (name, type_) = parens . group . align $
  fillBreak 1 (ppName name) <+> ppTypeAnnot type_

ppTypeAnnot :: Type -> Doc Highlight
ppTypeAnnot type_ = align $ ppColon <+> ppType type_

ppType :: Type -> Doc Highlight
ppType = ppTypePrec 0

ppTypePrec :: Int -> Type -> Doc Highlight
ppTypePrec _ TInt = ppTypeName "Int"
ppTypePrec _ TBool = ppTypeName "Bool"
ppTypePrec p (TArr ty tys) = parensIf (p > 0) $
  let
    ty' = ppTypePrec (succ p) ty
    tys' = ppTypePrec (succ p) <$> flatTArr tys
  in
  sep $ ty' : map (ppArrow <+>) tys'

ppTypeName :: Text -> Doc Highlight
ppTypeName = annotate HlType . pretty

ppArrow :: Doc Highlight
ppArrow = annotate HlSymbol "->"

ppExpr :: Expr -> Doc Highlight
ppExpr = ppExprPrec 0

ppExprPrec :: Int -> Expr -> Doc Highlight
ppExprPrec p = \case

  Var name -> ppName name

  Lit (LInt n) -> annotate HlLiteral (pretty n)
  Lit (LBool b) -> annotate HlLiteral (pretty b)

  Let name bound body -> parensIf (p > 0) $
    let
      let' = ppKeyword "let"
      name' = ppName name
      equals' = ppEquals
      bound' = ppExpr bound
      in' = ppKeyword "in"
      body' = ppExpr body

      nameBound =
        group $
          vsep [ name' <+> equals', indent 2 bound' ]
          `flatAlt`
          hsep [ name' <+> equals', bound' ]
      letIn =
        group $
          vsep [ let', indent 2 nameBound, in' ]
          `flatAlt`
          hsep [ let', nameBound, in' ]
    in
    group $
      vsep [ letIn, body' ]

  If cond true_ false_ -> parensIf (p > 0) $
    let
      if' = ppKeyword "if"
      cond' = ppExpr cond
      then' = ppKeyword "then"
      true' = ppExpr true_
      else' = ppKeyword "else"
      false' = ppExpr false_

      ifCondThen =
        group $
          vsep [ if', indent 2 cond', then' ]
          `flatAlt`
          hsep [ if', cond', then' ]
    in
    group $
      vsep [ ifCondThen, indent 2 true', else', indent 2 false' ]
      `flatAlt`
      hsep [ ifCondThen, true', else', false' ]

  BinaryOp op a b ->
    let
      op' = annotate HlBinaryOp $
        case op of
          Mul -> "*"
          Add -> "+"
          Sub -> "-"
          Eql -> "=="
      p' = opPrecedence op
      a' = ppExprPrec p' a
      b' = ppExprPrec (succ p') b
    in
    parensIf (p > p') $
      fillSep [ a', op' <+> b' ]

  app@App {} ->
    let
      (f, args) = unfoldApp app
      p' = appPrecedence
      f' = ppExprPrec p' f
      args' = ppExprPrec (succ p') <$> args
    in
    parensIf (p > p') $
      group . nest 2 . vsep $
        [ f', fillSep args' ]

  Lam arglist body -> parensIf (p > 0) $
    let
      lam' = ppLambda
      arglist' = ppArg <$> arglist
      arrow' = ppDoubleArrow
      body' = ppExpr body

      lamArrow' =
        group $
          vsep [ lam' <+> align (fillSep arglist'), arrow' ]
          `flatAlt`
          hsep [ lam', hsep arglist', arrow' ]
    in
    group $
      vsep [ lamArrow', indent 2 body' ]
      `flatAlt`
      hsep [ lamArrow', body' ]

ppKeyword :: Text -> Doc Highlight
ppKeyword = annotate HlKeyword . pretty

ppLambda :: Doc Highlight
ppLambda = annotate HlSymbol backslash

ppDoubleArrow :: Doc Highlight
ppDoubleArrow = annotate HlSymbol "=>"

ppColon :: Doc Highlight
ppColon = annotate HlSymbol colon

ppEquals :: Doc Highlight
ppEquals = annotate HlSymbol equals

parensIf :: Bool -> Doc Highlight -> Doc Highlight
parensIf True = parens . align
parensIf False = identity

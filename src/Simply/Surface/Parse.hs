module Simply.Surface.Parse
  ( parseText
  , parseText'
  , parseFile
  , parseFile'

  , keywords
  ) where

import Protolude hiding (Type, try)

import Control.Monad (fail)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as Lexer
import System.IO (hPutStrLn, stderr)

import Simply.Surface.AST


parseText :: Text -> Either (ParseError Char Dec) Program
parseText = parse pProgram "<input>"

parseText' :: Text -> IO Program
parseText' = dieOnParseError . parseText

parseFile :: FilePath -> IO (Either (ParseError Char Dec) Program)
parseFile filepath =
  parse pProgram filepath <$> Text.readFile filepath

parseFile' :: FilePath -> IO Program
parseFile' = dieOnParseError <=< parseFile

dieOnParseError :: Either (ParseError Char Dec) a -> IO a
dieOnParseError = either handleErr return
  where
    handleErr err = do
      hPutStrLn stderr "Parser Error:"
      hPutStrLn stderr $ parseErrorPretty err
      exitFailure


pProgram :: Parser Program
pProgram = label "program" $
  between pSpace eof program
  where
    program = Program <$> globals
    globals = many pGlobal

pGlobal :: Parser Global
pGlobal = label "global binding" $ do
  name <- pSymbol "def" *> pName
  arglist <- many pArgument
  type_ <- pTypeAnnot
  body <- pSymbol "=" *> pExpr
  pure $! Def name arglist type_ body

pArgument :: Parser Arg
pArgument = label "function argument" $
  parens $ do
    name <- pName
    type_ <- pTypeAnnot
    pure (name, type_)

pTypeAnnot :: Parser Type
pTypeAnnot = label "type annotation" $
  pSymbol ":" *> pType

pType :: Parser Type
pType = label "type" $
  makeExprParser term table
  where
    term = choice
      [ TInt <$ pSymbol "Int"
      , TBool <$ pSymbol "Bool"
      , parens pType
      ]
    table = [ [InfixR (TArr <$ pSymbol "->")] ]

pExpr :: Parser Expr
pExpr = label "expression" $
  makeExprParser term table
  where
    term = choice
      [ parens pExpr
      , pVar
      , pLit
      , pLet
      , pLam
      , pIf
      ]
    table =
      [ [ InfixL (pure App)
        ]
      , [ InfixL (BinaryOp Mul <$ pSymbol "*")
        ]
      , [ InfixL (BinaryOp Add <$ pSymbol "+")
        , InfixL (BinaryOp Sub <$ pSymbol "-")
        ]
      , [ InfixL (BinaryOp Eql <$ pSymbol "==")
        ]
      ]

pVar :: Parser Expr
pVar = label "variable" $
  Var <$> pName

pLit :: Parser Expr
pLit = label "literal" $
  choice
    [ Lit . LInt <$> pInt
    , Lit . LBool <$> pBool
    ]

pInt :: Parser Int
pInt = label "integer" $
  fromIntegral <$> lexeme Lexer.integer

pBool :: Parser Bool
pBool = label "boolean" $
  choice
    [ True <$ pSymbol "True"
    , False <$ pSymbol "False"
    ]

pLet :: Parser Expr
pLet = label "let-binding" $ do
  name <- pSymbol "let" *> pName
  bound <- pSymbol "=" *> pExpr
  body <- pSymbol "in" *> pExpr
  pure $! Let name bound body

pLam :: Parser Expr
pLam = label "lambda-expression" $ do
  arglist <- pSymbol "\\" *> some pArgument
  body <- pSymbol "=>" *> pExpr
  pure $! Lam arglist body

pIf :: Parser Expr
pIf = label "if-expression" $ do
  cond <- pSymbol "if" *> pExpr
  then_ <- pSymbol "then" *> pExpr
  else_ <- pSymbol "else" *> pExpr
  pure $! If cond then_ else_

pName :: Parser Name
pName = label "identifier" $
  lexeme . try $ do
    name <- parser
    when (isKeyword name) $
      fail $ "illegal identifier `" <> toS name <> "' is a keyword"
    pure name
  where
    parser = do
      firstChar <- lowerChar
      laterChars <- many $ alphaNumChar <|> oneOf [ '\'', '_' ]
      pure $! Text.pack (firstChar : laterChars)
    isKeyword = (`Set.member` keywords)

keywords :: Set Text
keywords = Set.fromList
  [ "def"
  , "let", "in"
  , "if", "then", "else"
  , "True", "False"
  ]

pSymbol :: String -> Parser String
pSymbol = Lexer.symbol pSpace

pSpace :: Parser ()
pSpace = Lexer.space (void spaceChar) lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "--"
    blockComment = Lexer.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme pSpace

parens :: Parser a -> Parser a
parens = label "parentheses" .
  between (pSymbol "(") (pSymbol ")")

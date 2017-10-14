module Haskit.Parser
  ( Definition(..)
  , Expr(..)
  , UnOp(..)
  , BinOp(..)
  , Constant(..)
  , definition
  , global
  ) where
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

data Definition = Definition
  { definitionName   :: String
  , definitionParams :: [String]
  , definitionBody   :: Expr
  } deriving (Show)

data Expr
  = Unary UnOp Expr
  | Binary BinOp Expr Expr
  | Constant Constant
  | Binding String Expr Expr
  | Call String [Expr]
  deriving (Show)

data UnOp
  = Not
  | Neg
  deriving (Show)

data BinOp
  = Greater
  | Less
  | Add
  | Sub
  | Mult
  | Div
  deriving (Show)

data Constant
  = Ident String
  | IntConst Integer
  | BoolConst Bool
  deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "let", "in", "true", "false", "not", "and", "or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

aExpr :: Parser Expr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser Expr
bExpr = makeExprParser bTerm bOperators

expr :: Parser Expr
expr = aExpr <|> bExpr <|> binding

aOperators :: [[Operator Parser Expr]]
aOperators =
  [ [Prefix (Unary Neg <$ symbol "-")]
  , [InfixL (Binary Mult <$ symbol "*"), InfixL (Binary Div <$ symbol "/")]
  , [InfixL (Binary Add <$ symbol "+"), InfixL (Binary Sub <$ symbol "-")]
  ]

bOperators :: [[Operator Parser Expr]]
bOperators =
  [ [Prefix (Unary Not <$ rword "not")] ]

aTerm :: Parser Expr
aTerm =
  parens aExpr <|>
  (identifier >>= (\ident -> call ident <|> (pure . Constant $ Ident ident))) <|>
  (Constant . IntConst) <$> integer

bTerm :: Parser Expr
bTerm =
  parens bExpr <|>
  ((Constant . BoolConst) True <$ rword "true") <|>
  ((Constant . BoolConst) False <$ rword "false") <|>
  rExpr

rExpr :: Parser Expr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (Binary op a1 a2)

relation :: Parser BinOp
relation = (symbol ">" *> pure Greater) <|> (symbol "<" *> pure Less)

binding :: Parser Expr
binding = do
  rword "let"
  sym <- identifier
  symbol "="
  value <- expr
  rword "in"
  body <- expr
  return $ Binding sym value body

call :: String -> Parser Expr
call sym = do
  symbol "("
  args <- expr `sepBy` char ','
  symbol ")"
  return $ Call sym args

definition :: Parser Definition
definition = do
  sym <- identifier
  params <- many identifier
  symbol "="
  body <- expr
  return $ Definition sym params body

global :: Parser [Definition]
global = many definition

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Haskit.Parser
    ( Expr(..)
    , UnOp(..)
    , BinOp(..)
    , Constant(..)
    , expr
    ) where
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

data Expr
    = Unary UnOp Expr
    | Binary BinOp Expr Expr
    | Constant Constant
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
    lineCmnt  = L.skipLineComment "//"
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
rws = ["if","then","else","let","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


aExpr :: Parser Expr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser Expr
bExpr = makeExprParser bTerm bOperators

expr :: Parser Expr
expr = aExpr <|> bExpr

aOperators :: [[Operator Parser Expr]]
aOperators =
  [ [Prefix (Unary Neg <$ symbol "-") ]
  , [ InfixL (Binary Mult <$ symbol "*")
    , InfixL (Binary Div <$ symbol "/") ]
  , [ InfixL (Binary Add <$ symbol "+")
    , InfixL (Binary Sub <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser Expr]]
bOperators =
  [ [Prefix (Unary Not <$ rword "not") ]
--   , [InfixL (Binary And <$ rword "and")
    -- , InfixL (Binary Or <$ rword "or") ]
  ]

aTerm :: Parser Expr
aTerm = parens aExpr
    <|> (Constant . Ident)      <$> identifier
    <|> (Constant . IntConst) <$> integer

bTerm :: Parser Expr
bTerm =  parens bExpr
    <|> ((Constant . BoolConst) True  <$ rword "true")
    <|> ((Constant . BoolConst) False <$ rword "false")
    <|> rExpr

rExpr :: Parser Expr
rExpr = do
    a1 <- aExpr
    op <- relation
    a2 <- aExpr
    return (Binary op a1 a2)

relation :: Parser BinOp
relation = (symbol ">" *> pure Greater)
    <|> (symbol "<" *> pure Less)

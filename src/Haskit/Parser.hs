{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Haskit.Parser
    ( BExpr(..)
    , BBinOp(..)
    , RBinOp(..)
    , AExpr(..)
    , ABinOp(..)
    , aExpr
    , bExpr) where
import           Data.Data                         (Data)
import           Data.Typeable                     (Typeable)
import           Data.Void
import           GHC.Generics                      (Generic)
import           Language.Haskell.TH.Lift.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer        as L
import           Text.Megaparsec.Expr

data BExpr
    = BoolConst Bool
    | Not BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Show)

data BBinOp
    = And
    | Or
    deriving (Show)

data RBinOp
    = Greater
    | Less
    deriving (Show)

data AExpr
    = Var String
    | IntConst Integer
    | Neg AExpr
    | ABinary ABinOp AExpr AExpr
    deriving (Show, Data, Typeable, Generic)

instance Lift AExpr where
  lift = genericLiftWithPkg CURRENT_PACKAGE_KEY

data ABinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show, Data, Generic)

instance Lift ABinOp where
  lift = genericLiftWithPkg CURRENT_PACKAGE_KEY

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
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators


aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
    <|> Var      <$> identifier
    <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm =  parens bExpr
    <|> (BoolConst True  <$ rword "true")
    <|> (BoolConst False <$ rword "false")
    <|> rExpr

rExpr :: Parser BExpr
rExpr = do
    a1 <- aExpr
    op <- relation
    a2 <- aExpr
    return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater)
    <|> (symbol "<" *> pure Less)

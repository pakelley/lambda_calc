module Parser (parseLC) where

import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Token as Tok

import Syntax

----------------------- lexer -----------------------
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["\\"]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

----------------------- definition -----------------------

parseLC :: String -> Either ParseError Expr
parseLC input = parse (contents expr) "<stdin>" input

----------------------- patterns -----------------------
----- top-level -----
expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

variable :: Parser Expr
variable = do
  x <- identifier 
  return (Var x) 

----- helpers -----

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> number
    <|> lambda  

----- primitive -----

number :: Parser Expr
number = do
    n <- natural
    return (Num  (fromIntegral n))

natural :: Parser Integer
natural = Tok.natural lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer
  
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

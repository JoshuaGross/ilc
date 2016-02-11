module Parser (parseExpr) where

import           Data.Char
import           Text.Parsec
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String   (Parser)

import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Token    as Tok

import           Syntax

import           Data.Either
import           Derive

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = ["derive", "true", "false", "and", "or", "not", "ifthenelse", "iszero", "pair", "first", "second", "nil", "succ", "pred", "plus", "mul", "pow", "sub"]
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- natural
  return (Lit (LInt (fromIntegral n)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Lam body args

church_true :: Parser Expr
church_true = do
  reservedOp "true"
  return $ head $ rights $ [parseExpr "(\\x y . x)"]

church_false :: Parser Expr
church_false = do
  reservedOp "false"
  return $ head $ rights $ [parseExpr "(\\x y . y)"]

church_and :: Parser Expr
church_and = do
  reservedOp "and"
  return $ head $ rights $ [parseExpr "(\\p q . p q p)"]

church_or :: Parser Expr
church_or = do
  reservedOp "or"
  return $ head $ rights $ [parseExpr "(\\p q . p p q)"]

church_not :: Parser Expr
church_not = do
  reservedOp "not"
  return $ head $ rights $ [parseExpr "(\\p a b . p b a)"]

church_ifthenelse :: Parser Expr
church_ifthenelse = do
  reservedOp "ifthenelse"
  return $ head $ rights $ [parseExpr "(\\p a b . p a b)"]

church_iszero :: Parser Expr
church_iszero = do
  reservedOp "iszero"
  return $ head $ rights $ [parseExpr "(\\n . n (\\x . false) true)"]

church_pair :: Parser Expr
church_pair = do
  reservedOp "pair"
  return $ head $ rights $ [parseExpr "(\\x y f . f x y)"]

church_first :: Parser Expr
church_first = do
  reservedOp "first"
  return $ head $ rights $ [parseExpr "(\\p . p true)"]

church_second :: Parser Expr
church_second = do
  reservedOp "second"
  return $ head $ rights $ [parseExpr "(\\p . p false)"]

church_nil :: Parser Expr
church_nil = do
  reservedOp "nil"
  return $ head $ rights $ [parseExpr "(\\x . true)"]

church_succ :: Parser Expr
church_succ = do
  reservedOp "succ"
  return $ head $ rights $ [parseExpr "(\\n f x . f (n f x))"]

church_pred :: Parser Expr
church_pred = do
  reservedOp "pred"
  return $ head $ rights $ [parseExpr "(\\n f x . n (\\g h . h (g f)) (\\u . x) (\\u . u))"]

church_plus :: Parser Expr
church_plus = do
  reservedOp "plus"
  return $ head $ rights $ [parseExpr "(\\m n f x . m succ n)"]

church_sub :: Parser Expr
church_sub = do
  reservedOp "sub"
  return $ head $ rights $ [parseExpr "(\\m n . n pred m)"]

church_mul :: Parser Expr
church_mul = do
  reservedOp "mul"
  return $ head $ rights $ [parseExpr "(\\m n f . m (n f))"]

church_pow :: Parser Expr
church_pow = do
  reservedOp "pow"
  return $ head $ rights $ [parseExpr "(\\b e . e b)"]

church_zero :: Parser Expr
church_zero = do
  reservedOp "0"
  return $ head $ rights $ [parseExpr "(\\f x . x)"]

church_derive :: Parser Expr
church_derive = do
  reservedOp "derive"
  t <- term
  return $ liftNonDerivativeTerms $ derive $ t

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> number
    <|> lambda
    <|> church_pair <|> church_first <|> church_second
    <|> church_true <|> church_false
    <|> church_and <|> church_or
    <|> church_not
    <|> church_zero
    <|> church_succ <|> church_pred <|> church_plus <|> church_sub <|> church_mul <|> church_pow
    <|> church_ifthenelse
    <|> church_iszero
    <|> church_derive

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

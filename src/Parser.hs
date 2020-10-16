{-# LANGUAGE FlexibleContexts #-}

module Parser (readExpr, readExprFile) where

import Control.Applicative hiding ((<|>))
import Control.Monad (mzero)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import LispVal
import Test.Hspec
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as L
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tk

-- Definition of our scheme
langDef :: Tk.GenLanguageDef T.Text () Identity
langDef =
  L.emptyDef
    { Tk.commentStart = "{-",
      Tk.commentEnd = "-}",
      Tk.opStart = mzero,
      Tk.opLetter = mzero,
      Tk.identStart = letter <|> oneOf "!$%&*/:<=>?^_~",
      Tk.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@",
      Tk.reservedOpNames = ["'", "\""]
    }

-- | Lexes streams of type `Text` with unit state over an `Identity` monad (no effects)
lexer :: Tk.GenTokenParser T.Text () Identity
lexer = Tk.makeTokenParser langDef

parseAtom :: Parser LispVal
parseAtom = do
  ident <- Tk.identifier lexer
  return $ Atom $ T.pack ident

sign :: Parser (Integer -> Integer)
sign = char '+' *> return id <|> char '-' *> return negate <|> return id

dec :: Parser Integer
dec = Tk.decimal lexer

parseNumber :: Parser LispVal
parseNumber = Number <$> (sign <*> dec)

parseList :: Parser LispVal
parseList = do
  elems <- concat <$> Text.Parsec.many parseExp `sepBy` (char ' ' <|> char '\n')
  return $ List elems

parseSExp :: Parser LispVal
parseSExp = Tk.parens lexer parseList

-- Example: 2 > '"x y z"
-- Result: "x y z"
parseQuote :: Parser LispVal
parseQuote = do
  Tk.reservedOp lexer "\'"
  parseExp

parseString :: Parser LispVal
parseString = do
  Tk.reservedOp lexer "\""
  s <- many1 $ noneOf "\""
  return $ String $ T.pack s

parseBool :: Parser LispVal
parseBool =
  char '#'
    *> ( (char 't' *> (return $ Bool True))
           <|> (char 'f' *> (return $ Bool False))
       )

parseExp :: Parser LispVal
parseExp = parseAtom <|> parseNumber <|> parseList <|> parseQuote <|> parseSExp <|> parseString <|> parseBool

contents :: Parser a -> Parser a
contents p = do
  Tk.whiteSpace lexer
  x <- p
  eof
  return x

type ReadExpression = T.Text -> Either ParseError LispVal

readExpr :: ReadExpression
readExpr = parse (contents parseExp) "<stdin>"

readExprFile :: ReadExpression
readExprFile = parse (contents parseList) "<file>"
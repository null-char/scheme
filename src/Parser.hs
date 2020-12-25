{-# LANGUAGE OverloadedStrings #-}

module Parser (readExpr, readExprFile) where

import Control.Applicative hiding ((<|>))
import Control.Monad (mzero)
import Data.Functor
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
      Tk.commentLine = "--",
      Tk.opStart = Tk.opLetter langDef,
      Tk.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~",
      Tk.identStart = letter <|> oneOf "-+/*=|&><",
      Tk.identLetter = digit <|> letter <|> oneOf "?+=|&-/",
      Tk.reservedOpNames = ["'", "\""]
    }

-- | Lexes streams of type `Text` with unit state over an `Identity` monad (no effects)
lexer :: Tk.GenTokenParser T.Text () Identity
lexer = Tk.makeTokenParser langDef

Tk.TokenParser {Tk.parens = parens, Tk.whiteSpace = ws} = lexer

reservedOp :: T.Text -> Parser ()
reservedOp op = Tk.reservedOp lexer $ T.unpack op

parseAtom :: Parser LispVal
parseAtom = do
  ident <- Tk.identifier lexer
  return $ Atom $ T.pack ident

parsePosNumber :: Parser LispVal
parsePosNumber = do
  char '+'
  d <- many1 digit
  return $ Number $ read d

parseNegNumber :: Parser LispVal
parseNegNumber = do
  char '-'
  d <- many1 digit
  return . Number . negate . read $ d

parseNormalNumber :: Parser LispVal
parseNormalNumber = Number . read <$> many1 digit

parseNumber :: Parser LispVal
parseNumber = parseNormalNumber <|> try parsePosNumber <|> try parseNegNumber

parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExp `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp = List <$> parens (parseExp `sepBy` ws)

parseReserved :: Parser LispVal
parseReserved =
  ((reservedOp "Nil" <|> reservedOp "()") >> return Nil)
    <|> (reservedOp "()" >> return Nil)
    <|> (reservedOp "#t" >> return (Bool True))
    <|> (reservedOp "#f" >> return (Bool False))

-- Example: 2 > '"x y z"
-- Result: "x y z"
parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  lsp <- parseExp
  return $ List [Atom "quote", lsp]

parseString :: Parser LispVal
parseString = do
  reservedOp "\""
  s <- many1 $ noneOf "\""
  reservedOp "\""
  return $ String $ T.pack s

parseExp :: Parser LispVal
parseExp = parseReserved <|> parseNumber <|> parseAtom <|> parseString <|> parseQuote <|> parseSExp

contents :: Parser a -> Parser a
contents p = do
  ws
  x <- p
  eof
  return x

type ReadExpression = T.Text -> Either ParseError LispVal

readExpr :: ReadExpression
readExpr = parse (contents parseExp) "<stdin>"

readExprFile :: SourceName -> ReadExpression
readExprFile = parse (contents parseList)
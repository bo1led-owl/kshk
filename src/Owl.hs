{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Owl (parse) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Text.Parsec.Prim as Parsec
import Text.ParserCombinators.Parsec hiding (parse)
import Tree

--   , ___
-- `\/{o,o}
--  / /)  )
-- /,--"-"-

inParens :: (GenParser Char st a) -> GenParser Char st a
inParens = between (char '(' *> spaces) (spaces *> char ')')

funcName :: GenParser Char st String
funcName = do
  first <- letter
  rest <- many alphaNum
  return (first : rest)

procName :: GenParser Char st String
procName = do
  char '@'
  many1 (try alphaNum <|> char '/')

def :: GenParser Char st Expr
def = inParens $
  do
    string "def"
    spaces
    name <- funcName
    spaces
    value <- expr
    return (Def name [] value)

flit :: GenParser Char st Expr
flit = do
  char '\''
  name <- funcName
  return $ FLit name

plit :: GenParser Char st Expr
plit = do
  char '\''
  name <- procName
  return $ PLit name

slit :: GenParser Char st Expr
slit = SLit <$> quoted chars
  where
    quoted = between (char '"') (char '"')
    chars = many (satisfy (/= '"'))

exec :: GenParser Char st Expr
exec = inParens $ do
  name <- (try (PLit <$> procName) <|> try (FLit <$> funcName) <|> expr)
  args <- many (spaces *> expr)
  return $ Exec name args

expr :: GenParser Char st Expr
expr = try def <|> try exec <|> try slit <|> try flit <|> plit

parse :: String -> Either ParseError Expr
parse = Parsec.parse expr ""

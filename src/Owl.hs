{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Owl (parse) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Text.Parsec.Prim as Parsec
import Text.ParserCombinators.Parsec hiding (parse)
import Tree

inParens :: (GenParser Char st a) -> GenParser Char st a
inParens = between (char '(' *> spaces) (spaces *> char ')')

def :: GenParser Char st Expr
def = inParens $
  do
    string "def"
    spaces
    name <- varName
    spaces
    value <- expr
    return (Def name value)
  where
    varName :: GenParser Char st String
    varName = do
      first <- letter
      rest <- many alphaNum
      return (first : rest)

cmd :: GenParser Char st Expr
cmd = undefined

lit :: GenParser Char st Expr
lit = undefined

expr :: GenParser Char st Expr
expr = try def <|> try cmd <|> lit

parse :: String -> Either ParseError Expr
parse = Parsec.parse expr ""


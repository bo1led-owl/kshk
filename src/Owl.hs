{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Owl (parse) where

import Data.List.NonEmpty
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

varName :: GenParser Char st String
varName = do
  first <- letter
  rest <- many alphaNum
  return (first : rest)

def :: GenParser Char st Expr
def = inParens $
  do
    string "def"
    spaces
    name <- varName
    spaces
    value <- expr
    return (Def name value)

cmd :: GenParser Char st Expr
cmd = inParens $
  do
    c <- expr
    spaces
    args <- many (spaces *> expr)
    return (Cmd (c :| args))

lit :: GenParser Char st Expr
lit = Lit <$> (try (quoted (many $ satisfy (/= '"'))) <|> many1 alphaNum)
  where
    quoted = between (char '"') (char '"')

varRef :: GenParser Char st Expr
varRef = VarRef <$> (char '$' *> varName)

expr :: GenParser Char st Expr
expr = try def <|> try cmd <|> try varRef <|> lit

parse :: String -> Either ParseError Expr
parse = Parsec.parse expr ""

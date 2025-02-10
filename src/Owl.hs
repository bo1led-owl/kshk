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

data Name = FuncName String | ProcName String deriving (Show)

inParens :: (GenParser Char st a) -> GenParser Char st a
inParens = between (char '(' *> spaces) (spaces *> char ')')

varName :: GenParser Char st String
varName = do
  first <- letter
  rest <- many alphaNum
  return (first : rest)

name :: GenParser Char st Name
name = try (FuncName <$> funcName) <|> ProcName <$> procName
  where
    funcName = varName
    procName = do
      char '@'
      many1 (try alphaNum <|> char '/')

def :: GenParser Char st Stmt
def = D <$> (try varDef <|> funcDef)

varDef :: GenParser Char st Def
varDef = inParens $
  do
    string "def"
    spaces
    n <- varName
    spaces
    value <- expr
    return (VarDef n value)

funcDef :: GenParser Char st Def
funcDef = inParens $
  do
    string "def"
    spaces
    n <- varName
    spaces
    args <- inParens (many (spaces *> varName))
    spaces
    value <- expr
    return (FuncDef n args value)

strLit :: GenParser Char st Expr
strLit = StrLit <$> quoted chars
  where
    quoted = between (char '"') (char '"')
    chars = many (satisfy (/= '"'))

varRef :: GenParser Char st Expr
varRef = VarRef <$> varName

call :: GenParser Char st Expr
call = inParens $ do
  n <- name
  spaces
  args <- many (spaces *> expr)
  return $
    case n of
      FuncName s -> FuncCall s args
      ProcName s -> ProcCall s args

expr :: GenParser Char st Expr
expr = try call <|> try varRef <|> strLit

stmt :: GenParser Char st Stmt
stmt = spaces *> (try def <|> (E <$> expr))

parse :: String -> Either ParseError Stmt
parse = Parsec.parse stmt ""

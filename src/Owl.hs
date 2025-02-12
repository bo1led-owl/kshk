{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Owl (parseForInteractive, parse) where

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
    funcName =
      try (string "+")
        <|> try (string "-")
        <|> try (string "*")
        <|> try (string "<=")
        <|> try (string ">=")
        <|> try (string "<")
        <|> try (string ">")
        <|> try (string "==")
        <|> try (string "!=")
        <|> varName
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

numLit :: GenParser Char st Expr
numLit = (NumLit . read) <$> (many1 digit)

boolLit :: GenParser Char st Expr
boolLit = BoolLit <$> (try true <|> false)
  where
    true = do string "#t"; return True
    false = do string "#f"; return False

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

ifExpr :: GenParser Char st Expr
ifExpr = inParens $ do
  string "if"
  spaces
  cond <- expr
  spaces
  lhs <- expr
  spaces
  rhs <- expr
  return (If cond lhs rhs)

expr :: GenParser Char st Expr
expr = try ifExpr <|> try call <|> try varRef <|> try boolLit <|> try strLit <|> numLit

stmt :: GenParser Char st Stmt
stmt = spaces *> (try def <|> (E <$> expr))

parse :: String -> Either ParseError Stmt
parse = Parsec.parse stmt ""

configOption = do
  char ':'
  varName

parseForInteractive :: String -> Either ParseError StmtOrOption
parseForInteractive = Parsec.parse (try (O <$> configOption) <|> (S <$> stmt)) ""

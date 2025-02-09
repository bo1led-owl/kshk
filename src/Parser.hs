module Parser (parse) where

import AST
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (parse)
import qualified Text.Parsec.Prim as Parsec (parse)

inParens :: (GenParser Char st a) -> GenParser Char st a
inParens = between (char '(') (char ')')

def :: GenParser Char st Expr
def = inParens $
  do
    spaces
    string "def"
    name <- varName
    value <- expr
    return (Cmd name args)
  where
    varName :: GenParser Char st Expr
    varName = do
      first <- letter
      rest <- many alphaNum
      return (first : rest)

cmd :: GenParser Char st Expr
cmd = undefined

lit :: GenParser Char st Expr
lit = undefined

parse :: GenParser Char st Expr
parse = try def <|> try cmd <|> lit

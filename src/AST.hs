module AST where

data Expr
  = Def String Expr
  | Cmd String [Expr]
  | Lit String
  deriving (Show)

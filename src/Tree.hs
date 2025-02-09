module Tree where

data Expr
  = Def String Expr
  | VarRef String
  | Cmd String [Expr]
  | Lit String
  deriving (Show)

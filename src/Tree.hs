module Tree where

data Expr
  = Def String [String] Expr
  | Exec Expr [Expr]
  | SLit String
  | FLit String
  | PLit String
  deriving (Show)

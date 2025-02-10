module Tree where

data Expr
  = VarDef String Expr
  | FuncDef String [String] Expr
  | FuncCall String [Expr]
  | ProcCall String [Expr]
  | VarRef String
  | StrLit String
  deriving (Show)

module Tree where

data Def
  = VarDef String Expr
  | FuncDef String [String] Expr
  deriving (Show)

data Stmt = E Expr | D Def deriving (Show)

data Expr
  = FuncCall String [Expr]
  | ProcCall String [Expr]
  | VarRef String
  | StrLit String
  deriving (Show)

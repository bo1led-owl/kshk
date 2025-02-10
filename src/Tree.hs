module Tree where

data Stmt = E Expr | D Def deriving (Show)

data Def
  = VarDef String Expr
  | FuncDef String [String] Expr
  deriving (Show)

data Expr
  = FuncCall String [Expr]
  | ProcCall String [Expr]
  | VarRef String
  | StrLit String
  | NumLit Int
  deriving (Show)

type Option = String

data StmtOrOption = S Stmt | O Option

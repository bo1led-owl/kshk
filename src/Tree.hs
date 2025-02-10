module Tree where

data Stmt = E Expr | D Def deriving (Show)

data Def
  = VarDef String Expr
  | FuncDef String [String] Expr
  deriving (Show)

data Expr
  = FuncCall String [Expr]
  | ProcCall String [Expr]
  | If Expr Expr Expr
  | VarRef String
  | StrLit String
  | NumLit Int
  | BoolLit Bool
  deriving (Show)

data Ret = I Int | Str String | B Bool deriving (Show)

type Option = String

data StmtOrOption = S Stmt | O Option

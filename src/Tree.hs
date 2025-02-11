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

retEq :: Ret -> Ret -> Bool
retEq (I x) (I y) = x == y
retEq (Str x) (Str y) = x == y
retEq (B x) (B y) = x == y
retEq _ _ = error "different types of arguments for comparison"

retLt :: Ret -> Ret -> Bool
retLt (I x) (I y) = x < y
retLt (Str x) (Str y) = x < y
retLt _ _ = error "wrong types of arguments for comparison"

type Option = String

data StmtOrOption = S Stmt | O Option

module Tree where

import qualified Data.Map as M

data Def
  = VarDef String Expr
  | FuncDef String [String] Expr

data Statement = E Expr | D Def

data Expr
  = FuncCall String [Expr]
  | ProcCall String [Expr]
  | VarRef String
  | StrLit String

data EState = State
  { vars :: M.Map String Expr,
    funcs :: M.Map String ([String], Expr)
  }

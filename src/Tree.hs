module Tree where

import Data.List.NonEmpty

data Expr
  = Def String Expr
  | VarRef String
  | Cmd (NonEmpty Expr)
  | Lit String
  deriving (Show)

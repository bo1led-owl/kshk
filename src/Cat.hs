module Cat () where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Spearfish (execCommand)
import Tree

--       /\_/\
--  /\  / o o \
-- //\\ \~(*)~/
-- `  \/   ^ /
--    | \|| ||
--    \ '|| ||
--     \)()-())

execExpr (StrLit s) st = return s
execExpr (ProcCall s e) st = execCommand s (sequenceA $ args e)
  where
    args (ex : exs) = execExpr ex st : args exs
    args [] = []
execExpr (FuncCall s e) st =
  if length e /= length names
    then error "incorrect amount of arguments given to a function"
    else execExpr fnExpr $ go names e st
  where
    (names, fnExpr) = case M.lookup s (funcs st) of
      Nothing -> error "reference to undefined func"
      Just f -> f
    go [] [] new_state = new_state
    go (s : strs) (e : expr) curState = go strs expr new_state
      where
        new_map = M.insert s e (vars curState)
        new_state = State {vars = new_map, funcs = funcs curState}
execExpr (VarRef s) st = executedRef
  where
    executedRef = execExpr lup st
    lup = case M.lookup s (vars st) of
      Nothing -> error "reference to undefined variable"
      Just expr -> expr

execDef :: Def -> EState -> EState
execDef (VarDef s e) st = State {vars = M.insert s e (vars st), funcs = funcs st}
execDef (FuncDef s names e) st = State {funcs = M.insert s (names, e) (funcs st), vars = vars st}

exec (D d) st = (execDef d st, return "")
exec (E e) st = (st, execExpr e st)

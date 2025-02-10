module Cat (exec, EState (State, vars, funcs)) where

import Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Spearfish
import System.Directory (getHomeDirectory, setCurrentDirectory)
import Tree

builtin = [("cd", cd)]

cd :: [Expr] -> EState -> IO String
cd e st = do
  dir <- new_dir
  setCurrentDirectory dir
  return ""
  where
    new_dir = case e of
      [] -> getHomeDirectory
      [x] -> execExpr x st
      _ -> error "Too many args for cd command"

data EState = State
  { vars :: M.Map String Expr,
    funcs :: M.Map String ([String], Expr)
  }

execExpr :: Expr -> EState -> IO String
execExpr (StrLit s) st = return s
execExpr (ProcCall s e) st = execCommand' s (sequenceA $ args e)
  where
    args (ex : exs) = execExpr ex st : args exs
    args [] = []
execExpr (FuncCall s e) st =
  case isBuiltin of
    Nothing -> defaultFunc
    Just builtin -> builtin e st
  where
    isBuiltin = L.lookup s builtin
    defaultFunc =
      if length e /= length names
        then error "incorrect amount of arguments given to a function"
        else execExpr fnExpr $ go names e st
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

execExprTopLevel :: Expr -> EState -> IO String
execExprTopLevel (ProcCall s e) st = do execCommand s (sequenceA $ args e); return ""
  where
    args (ex : exs) = execExpr ex st : args exs
    args [] = []
execExprTopLevel e st = execExpr e st

execDef :: Def -> EState -> EState
execDef (VarDef s e) st = State {vars = M.insert s e (vars st), funcs = funcs st}
execDef (FuncDef s names e) st = State {funcs = M.insert s (names, e) (funcs st), vars = vars st}

exec (D d) st = (execDef d st, return "")
exec (E e) st = (st, execExprTopLevel e st)

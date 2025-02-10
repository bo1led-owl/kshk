module Cat (exec, EState (ExecutorState, vars, funcs)) where

import Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Spearfish
import System.Directory (getHomeDirectory, setCurrentDirectory)
import Tree

builtin = [("cd", cd), ("+", plus)]

plus :: [Expr] -> EState -> IO Ret
plus es st = do
  xs <- args
  let ret =
        foldr
          ( \x acc -> case x of
              I n -> acc + n
              _ -> error "tried to sum not a number :)"
          )
          0
          xs
  return $ I ret
  where
    args = traverse (`execExpr` st) es

cd :: [Expr] -> EState -> IO Ret
cd e st = do
  dir <- new_dir
  setCurrentDirectory dir
  return $ Str ""
  where
    new_dir = case e of
      [] -> getHomeDirectory
      [x] -> show <$> execExpr x st
      _ -> error "Too many args for cd command"

data EState = ExecutorState
  { vars :: M.Map String Expr,
    funcs :: M.Map String ([String], Expr)
  }

execExpr :: Expr -> EState -> IO Ret
execExpr (NumLit n) st = return (I n)
execExpr (StrLit s) st = return (Str s)
execExpr (ProcCall s e) st = do
  let arg = args e
  let rets = sequenceA arg
  actual_arg_ret <- rets
  res <- execCommand' s (fmap show actual_arg_ret)
  return (Str res)
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
        new_state = ExecutorState {vars = new_map, funcs = funcs curState}
execExpr (VarRef s) st = executedRef
  where
    executedRef = execExpr lup st
    lup = case M.lookup s (vars st) of
      Nothing -> error "reference to undefined variable"
      Just expr -> expr

execExprTopLevel :: Expr -> EState -> IO Ret
-- execExpr (ProcCall s e) st = execCommand' s (fmap (\x -> fmap show x) $ args e)
execExprTopLevel (ProcCall s e) st = do
  let arg = args e
  let rets = sequenceA arg
  actual_arg_ret <- rets
  execCommand s $ fmap show actual_arg_ret
  return (Str "")
  where
    args (ex : exs) = execExpr ex st : args exs
    args [] = []
execExprTopLevel e st = execExpr e st

execDef :: Def -> EState -> EState
execDef (VarDef s e) st = st {vars = M.insert s e (vars st)}
execDef (FuncDef s names e) st = st {funcs = M.insert s (names, e) (funcs st)}

showRet (I i) = show i
showRet (Str s) = show s
showRet (B b) = if b then "#t" else "#f"

exec (D d) st = (execDef d st, return "")
exec (E e) st = (st, showRet <$> execExprTopLevel e st)

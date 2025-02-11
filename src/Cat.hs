module Cat (exec, EState (ExecutorState, vars, funcs)) where

import Data.Foldable
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Spearfish
import System.Console.Haskeline (outputStrLn)
import System.Directory (getHomeDirectory, setCurrentDirectory)
import Tree

builtin = [("cd", cd), ("+", plus), ("scons", scons), ("*", mult), ("-", minus), ("ГОЙДА", goida)]

goida :: [Expr] -> EState -> IO Ret
goida es st = return (Str "СВО")

minus :: [Expr] -> EState -> IO Ret
minus es st = do
  xs <- args
  let ret =
        foldl
          ( \acc x -> case x of
              I n -> acc - n
              _ -> error "tried to diff not a number :)"
          )
          0
          xs
  return $ I ret
  where
    args = traverse (`execExpr` st) es

mult :: [Expr] -> EState -> IO Ret
mult es st = do
  xs <- args
  let ret =
        foldl
          ( \acc x -> case x of
              I n -> acc * n
              _ -> error "tried to sum not a number :)"
          )
          1
          xs
  return $ I ret
  where
    args = traverse (`execExpr` st) es

scons :: [Expr] -> EState -> IO Ret
scons es st = do
  xs <- args
  let ret =
        foldl
          ( \acc x -> case x of
              Str n -> acc ++ n
              _ -> error "tried to sum not a string :)"
          )
          ""
          xs
  return $ Str ret
  where
    args = traverse (`execExpr` st) es

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
execExpr (BoolLit b) st = return (B b)
execExpr (ProcCall s e) st = do
  let arg = args e
  let rets = sequenceA arg
  actual_arg_ret <- rets
  res <- execCommand' s (map showRet actual_arg_ret)
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
execExpr (If cond lhs rhs) st = do
  r <- execExpr cond st
  if r == B True then execExpr lhs st else execExpr rhs st
  
execExprTopLevel :: Expr -> EState -> IO Ret
execExprTopLevel (ProcCall s e) st = do
  let arg = args e
  let rets = sequenceA arg
  actual_arg_ret <- rets
  execCommand s $ fmap showRet actual_arg_ret
  return (Str "")
  where
    args (ex : exs) = execExpr ex st : args exs
    args [] = []
execExprTopLevel e st = execExpr e st

execDef :: Def -> EState -> EState
execDef (VarDef s e) st = st {vars = M.insert s e (vars st)}
execDef (FuncDef s names e) st = st {funcs = M.insert s (names, e) (funcs st)}

showRet (I i) = show i
showRet (Str s) = if s == "" then "" else read $ show s
showRet (B b) = if b then "#t" else "#f"

exec (D d) st = (execDef d st, return "")
exec (E e) st = (st, showRet <$> execExprTopLevel e st)

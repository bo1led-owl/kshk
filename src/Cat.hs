module Cat (exec, EState (ExecutorState, vars, funcs)) where

import qualified Data.List as L
import qualified Data.Map as M
import Spearfish
import System.Directory (getHomeDirectory, setCurrentDirectory)
import Tree

builtins :: [(String, [Expr] -> EState -> IO Ret)]
builtins =
  [ ("cd", cd),
    ("+", plus),
    ("scons", scons),
    ("*", mult),
    ("-", minus),
    ("ГОЙДА", goida),
    ("==", eq),
    ("!=", neq),
    ("<", lt),
    (">", gt),
    ("<=", le),
    (">=", ge),
    ("pipe", layingPipes)
  ]

layingPipes :: [Expr] -> EState -> IO Ret
layingPipes es st = do
  let procs = processes
  let procsExecuted = map (\(name, proc) -> (name, fmap show <$> traverse (`execExpr` st) proc)) procs
  return (Str "")
  where
    processes =
      map
        ( \x -> case x of
            ProcCall s es -> (s, es)
            _ -> error "tried to pipe something but process"
        )
        es

goida :: [Expr] -> EState -> IO Ret
goida _ _ = return (Str "СВО")

minus :: [Expr] -> EState -> IO Ret
minus es st = do
  xs <- args
  let ret = case xs of
        [] -> error "incorrect amount of arguments"
        [I n] -> (-n)
        (I n : rest) ->
          foldl
            ( \acc x -> case x of
                I n -> acc - n
                _ -> error "tried to diff not a number"
            )
            n
            rest
  return $ I ret
  where
    args = traverse (`execExpr` st) $! es

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
    args = traverse (`execExpr` st) $! es

genericCmp :: (Ret -> Ret -> Bool) -> [Expr] -> EState -> IO Ret
genericCmp cmp (x : y : []) st = do
  lhs <- execExpr x st
  rhs <- execExpr y st
  return $ B $ cmp lhs rhs
genericCmp cmp (x : y : rest) st = do
  lhs <- execExpr x st
  rhs <- execExpr y st
  if not $ cmp lhs rhs
    then
      return $ B False
    else
      genericCmp cmp (y : rest) st
genericCmp _ _ _ = error "wrong amount of arguments for comparison"

eq :: [Expr] -> EState -> IO Ret
eq = genericCmp retEq

neq :: [Expr] -> EState -> IO Ret
neq = genericCmp (\x y -> not $ retEq x y)

lt :: [Expr] -> EState -> IO Ret
lt = genericCmp retLt

gt :: [Expr] -> EState -> IO Ret
gt = genericCmp (\x y -> (not $ retLt x y) && (not $ retEq x y))

le :: [Expr] -> EState -> IO Ret
le = genericCmp (\x y -> retLt x y || retEq x y)

ge :: [Expr] -> EState -> IO Ret
ge = genericCmp (\x y -> (not $ retLt x y))

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
    args = traverse (`execExpr` st) $! es

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
  { vars :: M.Map String Ret,
    funcs :: M.Map String ([String], Expr)
  }

execExpr :: Expr -> EState -> IO Ret
execExpr (NumLit n) _ = return (I n)
execExpr (StrLit s) _ = return (Str s)
execExpr (BoolLit b) _ = return (B b)
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
    isBuiltin = L.lookup s builtins
    defaultFunc =
      if length e /= length names
        then error "incorrect amount of arguments given to a function"
        else do
          args <- go names e st
          execExpr fnExpr args
    (names, fnExpr) = case M.lookup s (funcs st) of
      Nothing -> error "reference to undefined func"
      Just f -> f
    go :: [String] -> [Expr] -> EState -> IO EState
    go [] [] new_state = return new_state
    go (s : strs) (e : expr) curState = do
      r <- execExpr e curState
      let newMap = M.insert s r (vars curState)
      go strs expr (ExecutorState {vars = newMap, funcs = funcs curState})
    go _ _ _ = undefined
execExpr (VarRef s) st =
  return $ case M.lookup s (vars st) of
    Nothing -> error "reference to undefined variable"
    Just expr -> expr
execExpr (If cond lhs rhs) st = do
  r <- execExpr cond st
  if retEq r (B True) then execExpr lhs st else execExpr rhs st

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

execDef :: Def -> EState -> IO EState
execDef (VarDef s e) st = do
  r <- execExpr e st
  return $ st {vars = M.insert s r (vars st)}
execDef (FuncDef s names e) st = return $ st {funcs = M.insert s (names, e) (funcs st)}

showRet :: Ret -> String
showRet (I i) = show i
showRet (Str s) = if s == "" then "" else read $ show s
showRet (B b) = if b then "#t" else "#f"

exec :: Stmt -> EState -> (IO EState, IO String)
exec (D d) st = (execDef d st, return "")
exec (E e) st = (return st, showRet <$> execExprTopLevel e st)

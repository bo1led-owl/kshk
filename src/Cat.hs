module Cat (exec) where

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

execExprs :: M.Map String Expr -> [Expr] -> (M.Map String Expr, [IO String])
execExprs m (e : es) =
  let (new_map, str) = exec m e
      (newest_map, strs) = execExprs new_map es
   in (newest_map, str : strs)
execExprs m [] = (m, [])

exec :: M.Map String Expr -> Expr -> (M.Map String Expr, IO String)
exec vars (Def s e) =
  let new_vars = M.insert s e vars
   in exec new_vars e
exec vars (Cmd e) =
  let outstr = do
        let (m, strings) = execExprs m (toList e)
        let retstrs = sequenceA strings
        (s : args) <- retstrs
        execCommand s args
   in (vars, outstr)
exec vars (Lit str) = (vars, return str)
exec vars (VarRef name) = exec vars (fromMaybe (Lit "") (M.lookup name vars))

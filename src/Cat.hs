module Cat (exec) where

import Data.Functor ((<&>))
import Data.List (intersperse)
import qualified Data.Map as M
import Spearfish (execCommand)
import Tree

--       /\_/\
--  /\  / o o \
-- //\\ \~(*)~/
-- `  \/   ^ /
--    | \|| ||  Lara
--    \ '|| ||  Allen
--     \)()-())
-- nothing here for now

execExprs :: M.Map String Expr -> [Expr] -> (M.Map String Expr, [IO String])
execExprs m (e : es) =
  let (new_map, str) = exec m e
      (newest_map, strs) = execExprs new_map es
   in (newest_map, str : strs)
execExprs m [] = (m, [return ""])

exec :: M.Map String Expr -> Expr -> (M.Map String Expr, IO String)
exec vars (Def s e) =
  let new_vars = M.insert s e vars
   in exec new_vars e
exec vars (Cmd s e) =
  let outstr = do
        let (m, strings) = execExprs m e
        let retstrs = sequenceA strings
        args <- retstrs
        execCommand s args
   in (vars, outstr)
exec vars (Lit str) = (vars, return str)

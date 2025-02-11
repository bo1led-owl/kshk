{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Spearfish where

import System.IO
import System.Process
import Tree

--             ___
--          _ / __)_   °
-- _      .'_'-'\ /-'-. o °
-- \'-._.'-'\ / _\-(O)_: O
--  \ (__\/_ \ '._)  _\ o
--  /.' (_.'----''./'
--                '

execCommand :: String -> [String] -> IO ()
execCommand cmd args = do
  let proccess = proc cmd args
  (_, _, _, handle) <- createProcess proccess
  waitForProcess handle
  return ()

execCommand' :: String -> [String] -> IO String
execCommand' cmd args = do
  let proccess = (proc cmd args) {std_out = CreatePipe}
  (_, Just out, _, _) <- createProcess proccess
  hGetContents' out

pipin :: [(String, IO [String])] -> IO String
pipin xs = do
  return ""
  where
    go pr1 pr2 = do
      let (pr1name, pr1args) = pr1
      let (pr2name, pr2args) = pr2
      args1 <- pr1args
      args2 <- pr2args
      return ()

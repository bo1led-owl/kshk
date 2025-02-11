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

pipin :: [(String, IO [String])] -> IO ()
pipin xs = do
  go xs Nothing
  return ()
  where
    go [] h = return ()
    go [pr] h = do
      let (pr1name, pr1args) = pr
      args1 <- pr1args
      let new_proc = case h of
            Nothing -> createProcess (proc pr1name args1)
            Just h -> createProcess (proc pr1name args1) {std_in = UseHandle h}
      (_, _, _, handle) <- new_proc
      waitForProcess handle
      return ()
    go (pr1 : pr2 : pr3 : rest) h = do
      let (pr1name, pr1args) = pr1
      let (pr2name, pr2args) = pr2
      args1 <- pr1args
      args2 <- pr2args
      let new_proc = case h of
            Nothing -> createProcess (proc pr1name args1) {std_out = CreatePipe}
            Just h -> createProcess (proc pr1name args1) {std_out = CreatePipe, std_in = UseHandle h}
      (_, Just hout, _, _) <- new_proc
      (_, Just hout, _, _) <- createProcess (proc pr1name args1) {std_in = UseHandle hout, std_out = CreatePipe}
      go (pr3 : rest) (Just hout)
    go [pr1, pr2] h = do
      let (pr1name, pr1args) = pr1
      let (pr2name, pr2args) = pr2
      args1 <- pr1args
      args2 <- pr2args
      let new_proc = case h of
            Nothing -> createProcess (proc pr1name args1) {std_out = CreatePipe}
            Just h -> createProcess (proc pr1name args1) {std_out = CreatePipe, std_in = UseHandle h}
      (_, Just hout, _, _) <- new_proc
      (_, _, _, handle) <- createProcess (proc pr1name args1) {std_in = UseHandle hout}
      waitForProcess handle
      return ()

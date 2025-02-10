{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Spearfish where

import System.IO
import System.Process

--             ___
--          _ / __)_   °
-- _      .'_'-'\ /-'-. o °
-- \'-._.'-'\ / _\-(O)_: O
--  \ (__\/_ \ '._)  _\ o
--  /.' (_.'----''./'
--                '

execCommand :: String -> [String] -> IO ()
execCommand cmd args = do
  -- act_args <- args
  let proccess = proc cmd args
  (_, _, _, handle) <- createProcess proccess
  waitForProcess handle
  return ()

execCommand' :: String -> [String] -> IO String
execCommand' cmd args = do
  -- act_args <- args
  let proccess = (proc cmd args) {std_out = CreatePipe}
  (_, Just out, _, _) <- createProcess proccess
  hGetContents' out

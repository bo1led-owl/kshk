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

execCommand :: String -> [String] -> IO String
execCommand cmd args = do
  let proccess = (proc cmd args) {std_out = UseHandle stdout}
  (_, _, _, handle) <- createProcess proccess
  waitForProcess handle
  return ""

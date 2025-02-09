module Spearfish where

import System.IO
import System.Process

execCommand :: String -> [String] -> IO String
execCommand cmd args = do
  let proccess = (proc cmd args) {std_out = CreatePipe}
  (_, Just out, _, _) <- createProcess proccess
  hGetContents out

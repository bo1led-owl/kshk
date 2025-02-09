module Spearfish where

import System.IO
import System.Process

execCommand :: String -> [String] -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
execCommand cmd args = do
  let proccess = proc cmd args
  createProcess proccess

module Main where

import Cat
import Control.Arrow
import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.Map as M
import Owl
import System.Console.Haskeline
import System.Environment
import Tree

main :: IO ()
main = do
  env <- getEnvironment
  let startVars = M.fromList (map (second StrLit) env)
  runInputT defaultSettings (loop (State{ vars = startVars, funcs = M.empty }))
  where
    loop :: EState -> InputT IO ()
    loop st = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> return ()
        Just (input) -> case parse input of
          (Left err) -> do
            outputStrLn $ show err
            loop st
          (Right s) -> do
            let (newMap, output) = exec s st
            out <- liftIO output
            if not (L.null out) && last out /= '\n'
              then
                outputStrLn out
              else
                outputStr out
            loop newMap

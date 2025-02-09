module Main where

import Cat
import Control.Monad.IO.Class
import Data.Map
import Owl
import System.Console.Haskeline
import Tree

main :: IO ()
main = runInputT defaultSettings (loop empty)
  where
    loop :: Map String Expr -> InputT IO ()
    loop m = do
      minput <- getInputLine ""
      case minput of
        Nothing -> return ()
        Just input -> do
          case parse input of
            (Left err) -> outputStrLn $ show err
            (Right e) -> do
              let (newMap, output) = exec m e
              out <- liftIO output
              outputStr out
              loop newMap

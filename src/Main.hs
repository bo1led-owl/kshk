module Main where

import Cat
import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.Map as M
import Owl
import System.Console.Haskeline
import Tree

main :: IO ()
main = runInputT defaultSettings (loop M.empty)
  where
    loop :: M.Map String Expr -> InputT IO ()
    loop m = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> return ()
        Just input -> do
          case parse input of
            (Left err) -> do
              outputStrLn $ show err
              loop m
            (Right e) -> do
              let (newMap, output) = exec m e
              out <- liftIO output
              if not $ L.null out && last out /= '\n'
                then
                  outputStrLn out
                else
                  outputStr out
              loop newMap

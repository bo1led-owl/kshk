module Main where

import Cat
import Control.Arrow
import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.Map as M
import Owl
import System.Console.Haskeline
import System.Directory
import System.Environment
import Tree

data IState = InteractiveState
  { executorState :: EState,
    cwd :: Bool
  }

initState startVars =
  InteractiveState
    { executorState = ExecutorState {vars = startVars, funcs = M.empty},
      cwd = True
    }

getPrompt :: IState -> IO String
getPrompt st = do
  curDir <- getCurDir :: IO String
  return (curDir ++ " λ ")
  where
    getCurDir = if cwd st then getCurrentDirectory else return ""

main :: IO ()
main = do
  env <- getEnvironment
  let startVars = M.fromList (map (second StrLit) env)
  runInputT defaultSettings (loop (initState startVars))
  where
    loop :: IState -> InputT IO ()
    loop st = do
      dir <- liftIO getCurrentDirectory
      prompt <- liftIO $ getPrompt st
      minput <- getInputLine $ prompt
      case minput of
        Nothing -> return ()
        Just input -> do
          case parseForInteractive input of
            (Left err) -> do outputStrLn $ show err; loop st
            (Right stmtOrOpt) ->
              case stmtOrOpt of
                (O opt) ->
                  let newSt = case opt of "cwd" -> st {cwd = not $ cwd st}
                   in loop newSt
                (S s) -> do
                  let (newEState, output) = exec s (executorState st)
                  out <- liftIO output
                  if not (L.null out) && last out /= '\n'
                    then
                      outputStrLn out
                    else
                      outputStr out
                  loop st {executorState = newEState}

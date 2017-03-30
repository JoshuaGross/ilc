module Main where

import           Eval
import           Parser
import           Pretty
import           Syntax

import           Control.Monad
import           Control.Monad.Trans
import           System.Console.Haskeline

showStep :: (Int, Expr, Scope) -> IO ()
showStep (d, x, s) = putStrLn $ (replicate d ' ') ++ "=> " ++ ppexpr x ++ " -- scope: " ++ show s

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let (out, ~steps) = runEval ex
      putStrLn $ ppexpr ex
      mapM_ showStep steps
      print out

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Untyped> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

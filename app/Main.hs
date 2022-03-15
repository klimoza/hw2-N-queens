module Main where

import MyLib
import Picosat
import System.Environment

-- run "cabal run 'hw2-N-queens' N" to find the answer for N x N board

main :: IO ()
main = do
  args <- getArgs
  let n = read $ head args
  sol <- solve (generateFormula n)
  case sol of
    (Solution a) -> putStrLn $ printBoard a n
    _ -> putStrLn "I dunno..."

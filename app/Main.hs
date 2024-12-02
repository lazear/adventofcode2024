module Main where

import qualified Day2(run)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Day2.run "data/day2"

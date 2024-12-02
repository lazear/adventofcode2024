module Main where

-- import qualified MyLib (someFunc)
import qualified Day1(run)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Day1.run "data/day1"

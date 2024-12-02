module Day1(run) where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

parse :: String -> IO [[Int]]
parse = fmap (map (map read . words) . lines) . readFile

tuplify :: [[Int]] -> [(Int, Int)]
tuplify ([a,b]:xs) = (a, b) : tuplify xs
tuplify (_:xs) = tuplify xs
tuplify [] = []

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

counter :: [Int] -> Map.Map Int Int
counter = foldl (\ m k -> Map.insertWith (+) k 1 m) Map.empty

run :: String -> IO ()
run input = do
  -- test cases
  -- let a = sort [3, 4, 2, 1, 3, 3]
  -- let b = sort [4, 3, 5, 3, 9, 3]

  (a, b) <- mapTuple sort . unzip . tuplify <$> parse input
  let part1 = sum $ zipWith (\ a b -> abs (a - b)) a b
      counts = counter b
      part2 = sum $ map (\ k -> k * fromMaybe 0 (Map.lookup k counts)) a
  print part1
  print part2

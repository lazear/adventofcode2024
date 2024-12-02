module Day2(run) where

parse :: String -> IO [[Int]]
parse = fmap (map (map read . words) . lines) . readFile

deltas :: [Int] -> [Int]
deltas (a:b:xs) = b - a : deltas (b:xs)
deltas [_] = []
deltas [] = []

-- all levels are increasing, or all levels are decreasing (and non-zero)
-- max difference is 3
safe :: [Int] -> Bool
safe xs = (all (> 0) xs || all (< 0) xs) && all (\x -> abs x <= 3) xs

-- sublists with a different element removed
damper :: [Int] -> [[Int]]
damper xs = map (inner xs) [0..length xs]
  where inner xs ix = take (ix - 1) xs ++ drop ix xs

countTrue :: [Bool] -> Int
countTrue = foldl (\ acc x -> if x then acc + 1 else acc) 0

run :: String -> IO ()
run input = do
  xs <- parse input
  let 
    -- xs = [
    --   [7, 6, 4, 2, 1],
    --   [1, 2, 7, 8, 9],
    --   [9, 7, 6, 2, 1],
    --   [1, 3, 2, 4, 5],
    --   [8, 6, 4, 4, 1],
    --   [1, 3, 6, 7, 9]]

    safeReports = map (safe . deltas) xs
    safeReports2 = any (safe . deltas) <$> map damper xs

  print $ countTrue safeReports
  print $ countTrue safeReports2

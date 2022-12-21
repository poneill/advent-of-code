import Data.List (nub)

import Utils (getLines)



findMarkerPos :: Int -> String -> Int
findMarkerPos n cs
  | (length (nub (take n cs)) == n) = n
  | otherwise = 1 + findMarkerPos n (tail cs)

mainFunction :: String -> IO Int
mainFunction fname = do
  myLines <- getLines fname
  return $ findMarkerPos 4 (head myLines )

mainFunction2 :: String -> IO Int
mainFunction2 fname = do
  myLines <- getLines fname
  return $ findMarkerPos 14 (head myLines )

ans = mainFunction "src/aoc6/aoc6.txt"
ans2 = mainFunction2 "src/aoc6/aoc6.txt"

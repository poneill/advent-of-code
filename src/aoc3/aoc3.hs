import Data.List (elemIndex)
import Data.Set (fromList, toList, intersection)
import Data.Maybe (fromMaybe)
import Utils (getLines)


splitRucksack :: String -> (String, String)
splitRucksack rucksack = (take n rucksack, drop n rucksack)
  where n = div (length rucksack ) 2

findDuplicate :: (String, String) -> Char
findDuplicate (xs, ys) = head (toList $ intersection (fromList xs) (fromList ys))

findDuplicate2 :: [String] -> Char
findDuplicate2 xss = head (toList $ foldl1 intersection [fromList xs | xs <- xss])
getPriority :: Char -> Int
getPriority x = fromMaybe (error "char not found") (elemIndex x (['a'..'z'] ++ ['A'..'Z'])) + 1

mainFunction :: String -> IO Int
mainFunction fname = do
  lines <- getLines fname
  let scores = map (getPriority . findDuplicate . splitRucksack) lines
  return $ sum scores


testAns = mainFunction "aoc3_test.txt"
ans = mainFunction "aoc3.txt"

groupLines :: Int -> [a] -> [[a]]
groupLines n xs = groupLines' n xs cur acc
  where cur = []
        acc = []

groupLines' :: Int -> [a] -> [a] -> [[a]] -> [[a]]
groupLines' n [] cur acc = acc ++ [cur]
groupLines' n (x:xs) cur acc
  | length cur < n = groupLines' n xs (cur ++ [x]) acc
  | otherwise = groupLines' n xs [x] (acc ++ [cur])

getBadge :: [String] -> Char
getBadge = findDuplicate2

mainFunction2 :: String -> IO Int
mainFunction2 fname = do
  lines <- getLines fname
  let groups = groupLines 3 lines
  let scores = map (getPriority . getBadge) groups
  return $ sum scores


testAns2 = mainFunction2 "aoc3_test.txt"
ans2 = mainFunction2 "aoc3.txt"

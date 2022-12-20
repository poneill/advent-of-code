import Data.List (sort)

splitOn :: Eq a => [a] -> a -> [[a]]
splitOn xs s = splitOn' xs [] [] s
  where
    splitOn' :: Eq a => [a] -> [[a]] -> [a] -> a -> [[a]]
    splitOn' [] acc cur s = acc ++ [cur]
    splitOn' (x:xs) acc cur s
      | x == s = splitOn' xs (acc ++ [cur]) [] s
      | otherwise = splitOn' xs acc (cur ++ [x]) s

getLines :: String -> IO [String]
getLines fname = do
  str <- readFile fname
  return $ lines str

getElfCounts fname = do
  data' <- getLines fname
  let chunks = splitOn data' ""
  let calories = map (map (\x -> read x :: Int)) chunks
  let elfCounts = map sum calories
  return elfCounts

mainFunction1 fname = do
  elfCounts <- getElfCounts fname
  let maxElfCount = maximum elfCounts
  return maxElfCount

mainFunction2 fname = do
  elfCounts <- getElfCounts fname
  let sortedElfCounts = reverse $ sort elfCounts
  return $ sum (take 3 sortedElfCounts)
  

test1 :: IO Int
test1 = mainFunction1 "aoc1_test.txt"

main1 :: IO Int
main1 = mainFunction1 "aoc1.txt"


test2 :: IO Int
test2 = mainFunction2 "aoc1_test.txt"

main2 :: IO Int
main2 = mainFunction2 "aoc1.txt"

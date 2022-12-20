module Main (ans, main) where

import Utils (getLines)
import Data.List (intersect)
import Data.List.Split (splitOn)

type Interval = (Int, Int)

mainFunction :: String -> IO Int
mainFunction fname = do
  myLines <- getLines fname
  return $ length (filter (uncurry eitherContains . parseLine) myLines)

mainFunction2 :: String -> IO Int
mainFunction2 fname = do
  myLines <- getLines fname
  return $ length (filter (uncurry overlaps . parseLine) myLines)


parseInterval :: String -> Interval
parseInterval intervalStr = (xy !! 0, xy !! 1)
  where xy = map (\x -> read x :: Int) (splitOn "-" intervalStr)

parseLine :: String -> (Interval, Interval)
parseLine line = (xs, ys)
  where intervalStrs = splitOn "," line
        intervals = map parseInterval intervalStrs
        xs = intervals !! 0
        ys = intervals !! 1

intervalContains :: Interval -> Interval -> Bool
intervalContains (w, z) (x, y) = (w <= x) && (y <= z)

eitherContains :: Interval -> Interval -> Bool
eitherContains xs ys = intervalContains xs ys || intervalContains ys xs

overlaps :: Interval -> Interval -> Bool
overlaps xs ys = length (intersect xsList ysList) > 0
  where xsList = [(fst xs)..(snd xs)]
        ysList = [(fst ys)..(snd ys)]


testAns = mainFunction "src/aoc4/aoc4_test.txt"
testAns2 = mainFunction2 "src/aoc4/aoc4_test.txt"


ans = mainFunction "src/aoc4/aoc4.txt"
ans2 = mainFunction2 "src/aoc4/aoc4.txt"

main = undefined

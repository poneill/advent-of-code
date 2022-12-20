import Utils (getLines)
import Data.Maybe (fromMaybe)

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

type Round = (RPS, RPS)

beats :: RPS -> RPS -> Int
beats Rock Scissors = 1
beats Scissors Paper = 1
beats Paper Rock = 1
beats x y
  | x == y = 0
  | otherwise = - (beats y x)

interpret :: String -> RPS
interpret x = result 
  where
    table = [
      ("A", Rock),
      ("B", Paper),
      ("C", Scissors),
      ("X", Rock),
      ("Y", Paper),
      ("Z", Scissors)
      ]
    result = fromMaybe (error "foo") (lookup x table)


conditionFrom :: String -> Int
conditionFrom "X" = -1
conditionFrom "Y" = -0
conditionFrom "Z" = 1

interpret2 :: RPS -> String -> RPS
interpret2 rps conditionString = head [rps2 | rps2 <- [Rock, Paper, Scissors], beats rps2 rps == condition]
  where
    condition = conditionFrom conditionString

interpretLine :: String -> Round
interpretLine line = (interpret x, interpret y)
  where
    xy = words line
    x = xy !! 0
    y = xy !! 1

interpretLine2 :: String -> Round
interpretLine2 line = (rpsX, interpret2 rpsX y)
  where
    xy = words line
    x = xy !! 0
    y = xy !! 1
    rpsX = interpret x


bonusFrom :: RPS -> Int
bonusFrom Rock = 1
bonusFrom Paper = 2
bonusFrom Scissors = 3

scoreRound :: Round -> Int
scoreRound (oppMove, myMove) = score + bonusFrom myMove
  where
    score = (beats myMove oppMove + 1) * 3


mainFunction :: String -> IO Int
mainFunction fname = do
  lines <- getLines fname
  let scores = map (scoreRound . interpretLine) lines
  return $ sum scores

mainFunction2 :: String -> IO Int
mainFunction2 fname = do
  lines <- getLines fname
  let scores = map (scoreRound . interpretLine2) lines
  return $ sum scores

testAns = mainFunction "aoc2_test.txt"
ans = mainFunction "aoc2.txt"

testAns2 = mainFunction2 "aoc2_test.txt"
ans2 = mainFunction2 "aoc2.txt"

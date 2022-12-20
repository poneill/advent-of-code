module Aoc5 where

import Control.Lens
import Data.List (transpose)
import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Utils (getLines)

type Cargo = [[Char]]

data Instruction = Instruction Int Int Int  -- move QUANTITY from START to FINISH
  deriving Show

allButLast :: [a] -> [a]
allButLast = reverse.tail.reverse
  
(!!!) :: [a] -> [Int] -> [a]
(!!!) xs idxs = [x | (i, x) <- zip [0..] xs, i `elem` idxs]

removeSpaces :: String -> String
removeSpaces xs = filter (/= ' ') xs

parseCargoLines :: [String] -> Cargo
parseCargoLines cargoLines = transposedPayloads
  where idxLine = last cargoLines
        payloadLines = allButLast cargoLines
        idxs = getCargoIdxs idxLine
        payloads = [payloadLine !!! idxs | payloadLine <- payloadLines]
        transposedPayloads = map removeSpaces (transpose payloads)

parseInstructionLine :: String -> Instruction
parseInstructionLine line = Instruction quantity start finish
  where ws = words line
        quantity = read (ws !! 1) :: Int
        start = read (ws !! 3) :: Int
        finish = read (ws !! 5) :: Int

parseInstructionLines :: [String] -> [Instruction]
parseInstructionLines = map parseInstructionLine

readCargo :: Cargo -> String
readCargo = map head

mainFunction fname = do
  myLines <- getLines fname
  let groups = splitOn [""] myLines
  let cargoLines = groups !! 0
  let instructionLines = groups !! 1
  let cargo = parseCargoLines cargoLines
  let instructions = parseInstructionLines instructionLines
  let cargo' = executeMany instructions cargo                
  return $ readCargo cargo'

mainFunction2 fname = do
  myLines <- getLines fname
  let groups = splitOn [""] myLines
  let cargoLines = groups !! 0
  let instructionLines = groups !! 1
  let cargo = parseCargoLines cargoLines
  let instructions = parseInstructionLines instructionLines
  let cargo' = executeMany2 instructions cargo                
  return $ readCargo cargo'

testAns = mainFunction "src/aoc5/aoc5_test.txt"
ans = mainFunction "src/aoc5/aoc5.txt"

testAns2 = mainFunction2 "src/aoc5/aoc5_test.txt"
ans2 = mainFunction2 "src/aoc5/aoc5.txt"

  

        
  
getCargoIdxs :: String -> [Int]
getCargoIdxs line = [i | (i, c) <- zip [0..] line, c /= ' ']

pop :: Int -> Cargo -> (Char, Cargo)
pop n cargo = (c, cargo')
  where n' = (n - 1)
        stack = cargo !! n'
        c = head stack
        stack' = tail stack
        cargo' = cargo & (ix n') .~ stack'


push :: Char -> Int -> Cargo -> Cargo
push c n cargo = cargo'
  where n' = n - 1
        stack = cargo !! n'
        stack' = c : stack
        cargo' = cargo & (ix n') .~ stack'


        

execute :: Instruction -> Cargo -> Cargo
execute (Instruction 0 start finish) cargo = cargo
execute (Instruction n start finish) cargo = invariant `seq` execute instruction' cargo''
  where (c, cargo') = pop start (trace ("starting args: " ++ (show cargo) ++ (show (Instruction n start finish))) cargo)
        --instruction = trace "starting instruction:" (Instruction n start finish)
        cargo'' = push c finish cargo'
        instruction' = (Instruction (n - 1) start finish)
        before = sum $ map length cargo
        after = sum $ map length cargo''
        invariant = if before == after then "OK" else error (show (Instruction n start finish)) ++ " " ++ (show cargo)

pop2 :: Int -> Int -> Cargo -> ([Char], Cargo)
pop2 n start cargo = (cs, cargo')
  where start' = (start - 1)
        stack = cargo !! start'
        cs = take n stack
        stack' = drop n stack
        cargo' = cargo & (ix start') .~ stack'


push2 :: [Char] -> Int -> Cargo -> Cargo
push2 cs n cargo = cargo'
  where n' = n - 1
        stack = cargo !! n'
        stack' = cs ++ stack
        cargo' = cargo & (ix n') .~ stack'

execute2 :: Instruction -> Cargo -> Cargo
execute2 (Instruction n start finish) cargo = cargo''
  where (cs, cargo') = pop2 n start cargo
        cargo'' = push2 cs finish cargo'
  
executeMany :: [Instruction] -> Cargo -> Cargo
executeMany instructions cargo = foldl (flip execute) cargo instructions

executeMany2 :: [Instruction] -> Cargo -> Cargo
executeMany2 instructions cargo = foldl (flip execute2) cargo instructions

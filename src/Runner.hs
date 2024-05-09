module Runner where

import Data.Memory
import Data.Parser
import Parser

add :: Memory -> Memory -> Memory
add (MemNum a) (MemNum b) = MemNum (a+b)
add (MemStr a) (MemStr b) = MemStr (a<>b)
add (MemNum a) (MemStr b) = MemStr (show a<>b)
add (MemStr a) (MemNum b) = MemStr (a<>show b)
-- add _ _ = Right "Cannot add this and that"

mul :: Memory -> Memory -> Memory
mul (MemNum a) (MemNum b) = MemNum (a*b)
mul (MemNum a) (MemStr b) = MemStr (concat $ replicate a b)
mul (MemStr a) (MemNum b) = MemStr (concat $ replicate b a)

sub :: Memory -> Memory -> Memory
sub (MemNum a) (MemNum b) = MemNum (a-b)

execute :: AST -> Stack -> Stack
execute AstAdd stack = x : tail (tail stack)
  where x = add (head stack) (stack !! 1)
execute AstSub stack = x : tail (tail stack)
  where x = sub (head stack) (stack !! 1)
execute AstMul stack = (x : tail (tail stack))
  where x = mul (head stack) (stack !! 1)
execute AstInc ((MemNum x):xs) = MemNum (succ x) : xs
execute AstDec ((MemNum x):xs) = MemNum (pred x) : xs
execute AstSwap stack = (stack !! 1) : (stack !! 0) : tail (tail stack)
execute AstDup stack = head stack:head stack:tail stack
execute (AstPush (AstNum val)) stack = (MemNum val) : stack
execute AstPop (x:xs) = xs
execute _ stack = stack


splitOn :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitOn _ _ [] = ([],[])
splitOn elem pref (x:xs)
  | elem == x = (pref, xs)
  | otherwise = splitOn elem (pref++[x]) xs

runLoop :: [AST] -> Stack -> Stack
runLoop ast stack =
  if value > 0 then
    runLoop ast (runH ast stack)
  else stack
    where (MemNum value) = head stack

runH :: [AST] -> Stack -> Stack
runH (AstWhile:xs) stack =
  if value > 0 then
    runH afterloop (runLoop loop stack)
  else runH afterloop stack
    where (loop, afterloop) = splitOn AstFi [] xs
          (MemNum value) = head stack
runH [] stack = stack
runH [x] stack = execute x stack
runH (x:xs) stack = runH xs (execute x stack)

run commands = runH commands []

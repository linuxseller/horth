module Validator where

import Data.Parser
import Parser

data StackEl = SNum | SStr deriving Eq -- Stack = StackNum | StackString
type Stack = [StackEl]

add :: StackEl -> StackEl -> Either StackEl String
add SNum SNum = Left SNum
add SStr SStr = Left SStr
add SNum SStr = Left SStr
add SStr SNum = Left SStr
-- add _ _ = Right "Cannot add this and that"

mul :: StackEl -> StackEl -> Either StackEl String
mul SStr SStr = Right "Multiplication is undefined for str & str"
mul _ _ = Left SNum

sub :: StackEl -> StackEl -> Maybe String
sub SNum SNum = Nothing
sub _ _ = Just "Can substract only integers"

validCommand :: AST -> Stack -> Either Stack String
validCommand AstAdd stack
  | length stack < 2 = Right "Not enough values on stack: add"
  | otherwise = case restype of
                  Right err -> Right err
                  Left res  -> Left (res : xs)
    where restype = add x x'
          x = head stack
          x' = stack !! 1
          xs = drop 2 stack

validCommand AstSub stack
  | length stack < 2 = Right "Not enough values on stack: sub"
  | otherwise = case (x,x') of
                  (SNum, SNum) -> Left (SNum:xs)
                  _ -> Right "Can sub only int and int"
    where x = head stack
          x' = stack !! 1
          xs = drop 2 stack

validCommand AstMul stack
  | length stack < 2 = Right "Not enough values on stack: mul"
  | otherwise = case restype of
                  Right err -> Right err
                  Left res  -> Left (res : xs)
    where restype = mul x x'
          x = head stack
          x' = stack !! 1
          xs = drop 2 stack

validCommand AstInc stack@(SNum:_) = Left stack
validCommand AstInc _ = Right "Cant inc not num"

validCommand AstDec stack@(SNum:_) = Left stack
validCommand AstDec _ = Right "Cant dec not num"

validCommand AstSwap stack
  | length stack < 2 = Right "Not enough values on stack: swap"
  | otherwise = Left (stack !! 1 : head stack : drop 2 stack)

validCommand AstDup stack
  | null stack = Right "Not enough values on stack: dup"
  | otherwise = Left (head stack : head stack : tail stack)

validCommand (AstPush (AstNum val)) stack = Left (SNum : stack)

validCommand AstPop [] = Right "Stack empty: pop"
validCommand AstPop stack = Left $ tail stack

validCommand AstWhile stack = Right "while cant be called"
validCommand AstFi stack = Right "fi cant be called"

splitOn :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitOn _ _ [] = ([],[])
splitOn elem pref (x:xs)
  | elem == x = (pref, xs)
  | otherwise = splitOn elem (pref++[x]) xs

-- TODO: Fix loop checking
validateRunLoop :: [AST] -> Either Stack String -> Either Stack String
validateRunLoop ast (Left stack)
  | null stack = Right "Not enough values on stack: while"
  | head stack == SNum = Left stack
  | otherwise = Right "Not int on top of stack when loop"

validateRunH :: [AST] -> Either Stack String -> Either Stack String
validateRunH [] (Right err) = Right err
validateRunH [x] (Right err) = Right err
-- TODO: Fix loop checking
validateRunH (AstWhile:xs) (Left stack)
  | null stack = Right "Not enough values on stack: while"
  | head stack == SNum = Left stack
  | otherwise = Right "Not int on top of stack when loop"
validateRunH (x:xs) (Right err) = Right err
validateRunH [] (Left stack) = Left stack
validateRunH [x] (Left stack) = validCommand x stack
validateRunH (x:xs) (Left stack) = validateRunH xs (validCommand x stack)

validateRun commands =
  case (validateRunH commands $ Left []) of
    (Left _) -> Left "Validation finished succesfully"
    (Right err) -> Right $ "Validation failed: " <> err

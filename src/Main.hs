module Main where
import Parser
import Control.Applicative
import Data.Maybe
data Memory = MemNum Int | MemStr String
type Stack = [Memory]

instance Show Memory where
  show (MemNum a) = show a
  show (MemStr a) = a

add :: Memory -> Memory -> Either Memory String
add (MemNum a) (MemNum b) = Left $ MemNum (a+b)
add (MemStr a) (MemStr b) = Left $ MemStr (a<>b)
add (MemNum a) (MemStr b) = Left $ MemStr (show a<>b)
add (MemStr a) (MemNum b) = Left $ MemStr (a<>show b)
-- add _ _ = Right "Cannot add this and that"

mul :: Memory -> Memory -> Either Memory String
mul (MemNum a) (MemNum b) = Left $ MemNum (a*b)
mul (MemNum a) (MemStr b) = Left $ MemStr (concat $ replicate a b)
mul (MemStr a) (MemNum b) = Left $ MemStr (concat $ replicate b a)
mul (MemStr a) (MemStr b) = Right "Multiplication is undefined for str & str"

sub :: Memory -> Memory -> Either Memory String
sub (MemNum a) (MemNum b) = Left $ MemNum (a-b)
sub _ _ = Right "Can substract only integers"

smallStackErr = Right "Not enough elements on stack"

execute :: AST -> Stack -> Either Stack String
execute AstAdd rstack = if length rstack < 2 then Right "Not enough values on stack" else
                       case add (head rstack) (head $ tail rstack) of
                         Right err -> Right err
                         Left x -> Left $ x : tail (tail rstack)
execute AstSub rstack = if length rstack < 2 then Right "Not enough values on stack" else
                       case sub (head rstack) (head $ tail rstack) of
                         Right err -> Right err
                         Left x -> Left (x : tail (tail rstack))
execute AstMul rstack = if length rstack < 2 then Right "Not enough values on stack" else
                       case mul (head rstack) (head $ tail rstack) of
                         Right err -> Right err
                         Left x -> Left (x : tail (tail rstack))
execute AstInc (x:xs) =
  case x of
    MemNum a -> Left $ (MemNum $ a+1) : xs
    otherwise -> Right "Could not inc not num"

execute AstDec (x:xs) =
  case x of
    MemNum a -> Left $ (MemNum $ a-1) : xs
    otherwise -> Right "Could not dec not num"

execute AstSwap rstack = if runnable rstack then Left $ (rstack !! 1) : (rstack !! 0) : tail (tail rstack) else smallStackErr
  where runnable = \x -> length x >=2

execute AstDup rstack = if runnable then Left (head rstack:head rstack:tail rstack) else smallStackErr
  where runnable = not $ null rstack

execute (AstPush (AstNum val)) rstack = Left $ (MemNum val) : rstack

execute AstPop [] = Right "Stack empty for pop"
execute AstPop (x:xs) = Left xs

execute _ rstack = Left rstack


splitOn :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitOn _ _ [] = ([],[])
splitOn elem pref (x:xs)
  | elem == x = (pref, xs)
  | otherwise = splitOn elem (pref++[x]) xs

runLoop :: [AST] -> Either Stack String -> Either Stack String
runLoop ast (Left stack) =
  if value > 0 then
    runLoop ast (runH ast $ Left stack)
  else Left stack
    where (MemNum value) = head stack

runH :: [AST] -> Either Stack String -> Either Stack String
runH [] (Right err) = Right err
runH [x] (Right err) = Right err
runH (AstWhile:xs) (Left stack) =
  if value > 0 then
    runH afterloop (runLoop loop (Left stack))
  else runH afterloop $ Left stack
    where (loop, afterloop) = splitOn AstFi [] xs
          (MemNum value) = head stack
runH (x:xs) (Right err) = Right err
runH [] (Left stack) = Left stack
runH [x] (Left stack) = execute x stack
runH (x:xs) (Left stack) = runH xs (execute x stack)

run commands = runH commands $ Left []

main :: IO ()
main = do
  -- prg <- readFile "program.horth"
  let prg = "5 while dup dec fi"
  let (rest, program) = fromMaybe ("", []) $ runParser (many $ ws*>parseAst) prg
  print program
  print $ run program

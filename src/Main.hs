module Main where
data Command = Add | Sub | Mul | Swap | Print | Push Memory deriving Show
data Memory = MyNum Integer | MyStr String deriving Show
type Stack = [Memory]

add :: Memory -> Memory -> Either Memory String
add (MyNum a) (MyNum b) = Left $ MyNum (a+b)
add (MyStr a) (MyStr b) = Left $ MyStr (a<>b)
add (MyNum a) (MyStr b) = Left $ MyStr (show a<>b)
add (MyStr a) (MyNum b) = Left $ MyStr (a<>show b)
-- add _ _ = Right "Cannot add this and that"

mul :: Memory -> Memory -> Either Memory String
mul (MyNum a) (MyNum b) = Left $ MyNum (a*b)
mul (MyNum a) (MyStr b) = Left $ MyStr (concat $ replicate (fromInteger a) b)
mul (MyStr a) (MyNum b) = Left $ MyStr (concat $ replicate (fromInteger b) a)
mul (MyStr a) (MyStr b) = Right "Multiplication is undefined for str & str"

sub :: Memory -> Memory -> Either Memory String
sub (MyNum a) (MyNum b) = Left $ MyNum (a-b)
sub _ _ = Right "Can substract only integers"

execute :: Command -> Stack -> Either Stack String
execute Add rstack = if length rstack < 2 then Right "Not enough values on stack" else
                       case add (head rstack) (head $tail rstack) of
                         Right err -> Right err
                         Left x -> Left $ x : tail (tail rstack)
execute Sub rstack = if length rstack < 2 then Right "Not enough values on stack" else
                       case sub (head rstack) (head $tail rstack) of
                         Right err -> Right err
                         Left x -> Left (x : tail (tail rstack))
execute Mul rstack = if length rstack < 2 then Right "Not enough values on stack" else
                       case mul (head rstack) (head $tail rstack) of
                         Right err -> Right err
                         Left x -> Left (x : tail (tail rstack))
execute Print rstack = undefined
execute Swap rstack = Left $ head (tail rstack) : head rstack : tail (tail rstack)
execute (Push val) rstack = Left $ val : rstack


runH :: [Command] -> Either Stack String -> Either Stack String
runH [] (Right err) = Right err
runH [x] (Right err) = Right err
runH (x:xs) (Right err) = Right err
runH [] (Left stack) = Left stack
runH [x] (Left stack) = execute x stack
runH (x:xs) (Left stack) = runH xs (execute x stack)

run commands = runH commands $ Left []

main :: IO ()
main = do
    print "Hello World"

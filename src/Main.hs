module Main where

import Parser
import Control.Applicative
import Data.Maybe
import System.Environment

import Data.Memory
import Data.Parser
import Validator
import Runner
import Compiler

programToAst code = snd $ fromMaybe ("", []) $ runParser (many $ ws*>parseAst) code

main :: IO ()
main = do
  args <- getArgs
  prg <- readFile $ head args
  let (rest, program) = fromMaybe ("", []) $ runParser (many $ ws*>parseAst) prg
  -- print program
  let validationResult = validateRun program
  case validationResult of
    (Right a) -> print a
    (Left a) -> putStrLn $ compile program
  -- print $ run program

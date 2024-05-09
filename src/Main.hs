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
  [filename, "-t", mode] <- getArgs
  prg <- readFile $ filename
  let (rest, program) = fromMaybe ("", []) $ runParser (many $ ws*>parseAst) prg
  let validationResult = validateRun program
  case validationResult of
    (Right a) -> print a
    _ ->
      case mode of
        "run" -> putStrLn $ show $ run program
        "compile" -> putStrLn $ compile program

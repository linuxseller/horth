module Main where

import Parser
import Control.Applicative
import Data.Maybe
import Data.Memory
import Data.Parser
import Validator
import Runner

programToAst code = snd $ fromMaybe ("", []) $ runParser (many $ ws*>parseAst) code

main :: IO ()
main = do
  -- prg <- readFile "program.horth"
  let prg = "5 while dup dec fi"
  let (rest, program) = fromMaybe ("", []) $ runParser (many $ ws*>parseAst) prg
  print program
  let validationResult = validateRun program
  case validationResult of
    (Right a) -> print a
    (Left a) -> print $ run program
  -- print $ run program

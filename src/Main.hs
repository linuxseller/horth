module Main where

import Parser
import Control.Applicative
import Data.Maybe
import Data.Memory
import Runner

programToAst code = snd $ fromMaybe ("", []) $ runParser (many $ ws*>parseAst) code

main :: IO ()
main = do
  -- prg <- readFile "program.horth"
  let prg = "5 while dup dec fi"
  let (rest, program) = fromMaybe ("", []) $ runParser (many $ ws*>parseAst) prg
  print program
  print $ run program

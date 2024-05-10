module Main where

import Parser
import Control.Applicative
import Data.Maybe
import System.Environment
import System.Exit
import System.Process

import Data.Memory
import Data.Parser
import Validator
import Runner
import Compiler

programToAst code = snd $ fromMaybe ("", []) $ runParser (many $ ws*>parseAst) code

getMode :: [String] -> String
getMode [] = "compile"
getMode [_] = "compile"
getMode (f:m:xs)
  | f == "-m" = m
  | otherwise = getMode (m:xs)

getOutfile :: [String] -> String
getOutfile [] = "a"
getOutfile [_] = "a"
getOutfile (f:m:xs)
  | f == "-o" = m
  | otherwise = getOutfile (m:xs)

main :: IO ()
main = do
  -- [filename, "-t", mode] <- getArgs
  args <- getArgs
  let mode = getMode args
  let filename = head args
  let outfile = getOutfile args
  prg <- readFile $ filename
  let (rest, program) = fromMaybe ("", []) $ runParser (many $ ws*>parseAst) prg
  putStrLn $ "parsed program: " <> show program
  let validationResult = validateRun program
  putStrLn "Validating..."
  case validationResult of
    (Right a) -> do
      print a
      exitWith $ ExitFailure 1
    _ -> putStrLn "Validation: success"
  case mode of
    "run" -> do
      putStrLn "Running..."
      putStrLn $ show $ run program
    "compile" -> do
      writeFile (outfile <> ".asm") $ compile program
      putStrLn "Compiling..."
      callProcess "as" [outfile<>".asm", "-o", outfile<>".o"]
      putStrLn "Linking..."
      callProcess "ld" [outfile<>".o", "-o", outfile]
      callProcess "rm" [outfile<>".o"]
      putStrLn "Done!"

module Data.Parser where

import Control.Applicative

data AstValue = AstNum Int | AstStr String deriving (Show, Eq)

data AST
  = AstPush AstValue
  | AstPop
  | AstAdd
  | AstDup
  | AstSub
  | AstInc
  | AstDec
  | AstMul
  | AstPrint
  | AstSwap
  | AstWhile
  | AstFi
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', b) <- p2 input'
      Just (input'', f b)

instance Alternative Parser where
  empty = Parser $ \input -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> do p1 input <|> p2 input

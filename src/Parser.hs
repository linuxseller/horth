module Parser where
import Control.Applicative
import Data.Char

-- ghci> runParser (many (ws *> parseAst))  "10inc20"

data AstValue = AstNum Int | AstStr String deriving (Show, Eq)

data AST
  = AstPush AstValue
  | AstAdd
  | AstSub
  | AstInc
  | AstDec
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

astAdd :: Parser AST
astAdd = AstAdd <$ stringP "add"

astSub :: Parser AST
astSub = AstSub <$ stringP "sub"

astInc :: Parser AST
astInc = AstInc <$ stringP "inc"

astDec :: Parser AST
astDec = AstDec <$ stringP "dec"

astNum :: Parser AST
astNum = (\dgs -> AstPush $ AstNum $ read dgs) <$> notNull (spanP isDigit)

astStr :: Parser AST
astStr = undefined -- Ast <$ stringP ""

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = undefined

parseAst = astDec <|> astInc <|> astAdd <|> astSub <|> astNum -- <|> astStr

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                (input', xs) <- p input
                if null xs then Nothing else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (pred, suff) = span f input in Just (suff, pred)

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys) | y == x = Just (ys, x)
             | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP x = sequenceA $ map charP x

module Parser where
import Control.Applicative
import Data.Char
import Data.Parser

astAdd :: Parser AST
astAdd = AstAdd <$ stringP "add"

astSub :: Parser AST
astSub = AstSub <$ stringP "sub"

astInc :: Parser AST
astInc = AstInc <$ stringP "inc"

astDec :: Parser AST
astDec = AstDec <$ stringP "dec"

astMul :: Parser AST
astMul = AstMul <$ stringP "mul"

astSwap :: Parser AST
astSwap = AstSwap <$ stringP "swap"

astDup :: Parser AST
astDup = AstDup <$ stringP "dup"

astWhile :: Parser AST
astWhile = AstWhile <$ stringP "while"

astFi :: Parser AST
astFi = AstFi <$ stringP "fi"

astPop :: Parser AST
astPop = AstPop <$ stringP "pop"

astNum :: Parser AST
astNum = (\dgs -> AstPush $ AstNum $ read dgs) <$> notNull (spanP isDigit)

astStr :: Parser AST
astStr = undefined -- Ast <$ stringP ""

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = undefined

parseAst = astDec <|> astInc <|> astAdd <|> astSub <|> astMul <|> astSwap <|> astNum <|> astDup <|> astWhile <|> astFi <|> astPop -- <|> astStr

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

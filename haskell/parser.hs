module Main where

import           Control.Applicative
import           Data.Char

-- Can use something like Either to get better error reporting.
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)  -- like a struct in c.
  }

instance Functor Parser where 
  --fmap :: a -> b -> Parser a -> Parser b 
  fmap f (Parser p) = Parser $ \input -> 
    do  
      (input', x) <- p input 
      Just (input', f x)
instance Applicative Parser where 
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> 
    do (input', f)  <- p1 input
       (input'', x) <- p2 input'
       Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing 
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input 


charP :: Char -> Parser Char
charP x = Parser $ f
  where
    f [] = Nothing
    f (y:ys)
      |    y == x = Just (ys, x)
      | otherwise = Nothing


stringP :: String -> Parser String 
stringP = sequenceA . map charP 

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input 
                              in Just (rest, token)

notNull :: Parser [a] -> Parser [a] 
notNull p = Parser $ \input -> 
  do (input', xs) <- runParser p input 
     if null xs 
        then Nothing 
        else Just (input', xs)

-- NOTE: no escape support
stringLiteral :: Parser String 
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

sepBy :: Parser a -> Parser b -> Parser [b] 
sepBy sep element = (:) <$> element <*> many (sep *> element)  <|> pure [] 

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do 
  input   <- readFile fileName
  return (snd <$> runParser parser input)
  
  

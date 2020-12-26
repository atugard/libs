module Parser where

import           Control.Applicative
import           Data.Char

flags = ['l','c','r','h']

-- Can use something like Either to get better error reporting.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)  -- like a struct in c.
  }

instance Functor Parser where 
  --fmap :: a -> b -> Parser a -> Parser b 
  fmap f (Parser p) = Parser $ \input -> 
    do  
      (x, input') <- p input 
      Just (f x, input')
instance Applicative Parser where 
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> 
    do (f, input')  <- p1 input
       (x, input'') <- p2 input'
       Just (f x, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing 
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input 


charBoolP :: (Char -> Bool) -> Parser Char 
charBoolP p = Parser $ f 
  where 
    f [] = Nothing 
    f (y:ys)
      | p y       = Just (y, ys)
      | otherwise = Nothing 
     

foldAlt :: [Parser a] -> Parser a 
foldAlt [] = empty 
foldAlt (p:ps) = p <|> foldAlt ps 

flagP :: Parser Char
flagP = ws *> charP '-' *> foldAlt (map (\x -> (charBoolP (== x))) flags)  <* ws

numberP :: Parser Integer
numberP = Parser $ f 
  where 
    f [] = Nothing 
    f input = 
      do (x, input') <- runParser (spanP isDigit) input
         if null x 
            then Nothing 
            else Just (read x:: Integer, input')

numbersP :: Parser [Integer]
numbersP = sepBy ws numberP



charP :: Char -> Parser Char 
charP x = charBoolP (== x)

stringP :: String -> Parser String 
stringP = sequenceA . map charP 

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just (span f input)

ws :: Parser String 
ws = spanP (== ' ')

notNull :: Parser [a] -> Parser [a] 
notNull p = Parser $ \input -> 
  do (xs, input') <- runParser p input 
     if null xs 
        then Nothing 
        else Just (xs, input')

-- NOTE: no escape support
stringLiteral :: Parser String 
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

sepBy :: Parser a -> Parser b -> Parser [b] 
sepBy sep element = (:) <$> element <*> many (sep *> element)  <|> pure [] 

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do 
  input   <- readFile fileName
  return (fst <$> runParser parser input)



module Parsers
  ( parse
  , parseCSV
  , parseINI
  ) where

import Data.Char
import Data.Maybe

data Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap f p = P $ fa . runParser p
    where fa m = case m of
                   Just (a, s) -> Just (f a, s)
                   _ -> Nothing

instance Applicative Parser where
  pure = pureParser
  fp <*> fx = P p
    where p input = case runParser fp input of
                      Just (f, s) -> case runParser fx s of
                                       Just (a, s') -> Just (f a, s')
                                       _ -> Nothing
                      _ -> Nothing

instance Monad Parser where
  return = pureParser
  fa >>= k = P $ \input -> do
    (a, s)  <- runParser fa input
    runParser (k a) s

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = case runParser p s of
  Just (res, "") -> Just res
  _ -> Nothing

noParser :: Parser a
noParser = P $ const Nothing

pureParser :: a -> Parser a
pureParser a = P $ \input -> Just (a, input)

anyChar :: Parser Char
anyChar = P f
  where f (h:t) = Just (h, t)
        f _ = Nothing

char :: Char -> Parser ()
char c = anyChar >>= f
  where f x | x == c = pureParser () | otherwise = noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = anyChar >>= f
  where f x | x /= c = pureParser x | otherwise = noParser

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P $ \input -> case runParser p1 input of
                               Just o -> Just o
                               Nothing -> runParser p2 input

many :: Parser a -> Parser [a]
many p = (p >>= \x -> many p >>= \xs -> return (x:xs)) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = (p1 >>= \x -> many (p2 >>= const p1) >>= \xs -> return (x:xs)) `orElse` return []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
      char '"'
      content <- many (anyCharBut '"')
      char '"'
      return content

type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

many1 :: Parser a -> Parser [a]
many1 p = p >>= \x -> many p >>= \xs -> return (x:xs)

letterOrDigit :: Parser Char
letterOrDigit = do
  c <- anyChar
  if isAlphaNum c then return c else noParser

chars :: Char -> Parser ()
chars c = do
  _ <- many $ char c
  return ()

parseINI :: Parser INIFile
parseINI = many1 parseSection
  where
    parseSection = (,) <$> header <*> (catMaybes <$> many (declaration `orElse` comment)) `orElse` return []
    header = do
      char '['
      name <- many1 letterOrDigit
      char ']'
      _ <- chars '\n'
      return name
    declaration = do
      identifier <- many1 letterOrDigit
      _ <- chars ' '
      _ <- char '='
      _ <- chars ' '
      value <- many $ anyCharBut '\n'
      _ <- chars '\n'
      return $ Just (identifier,value)
    comment = do
      _ <- char '#'
      _ <- many $ anyCharBut '\n'
      _ <- chars '\n'
      return Nothing

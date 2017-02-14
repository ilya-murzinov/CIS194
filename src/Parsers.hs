module Parsers
  ( runParser
  , parse
  ) where

data Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap f p = P $ fa . runParser p
    where fa m = case m of
                   Just (a, s) -> Just (f a, s)
                   _-> Nothing

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = case runParser p s of
  Just (res, "") -> Just res
  _ -> Nothing

noParser :: Parser a
noParser = P $ const Nothing

pureParser :: a -> Parser a
pureParser a = P f
  where f "" = Just (a, "")
        f _ = Nothing

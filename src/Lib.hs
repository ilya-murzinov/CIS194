{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( halveEvens
  , safeString
  , holes
  , longestText
  , adjacents
  , commas
  , addPolynomials
  , sumNumbers
  , wordCount
  ) where

import           Data.Char
import           Data.List
import           Data.Ord

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter (\i -> i `mod` 2 == 0)

safeString :: String -> String
safeString = map replace
  where replace c | isControl c = '_'
                  | isAscii c   = c
                  | otherwise   = '_'

holes :: [a] -> [[a]]
holes l = reverse [x | x <- subsequences l, length x == length l - 1]

longestText :: Show a => [a] -> a
longestText = maximumBy $ comparing $ length . show

adjacents :: [a] -> [(a,a)]
adjacents l = zip l $ tail l

commas :: [String] -> String
commas = intercalate ", "

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1 $ zipWith (+)

sumNumbers :: String -> Integer
sumNumbers = sum . map read . filter (isDigit . head) .
  groupBy (\c1 c2 -> isDigit c1 && isDigit c2)

-- DOESN'T WORK
sumNumbers' :: String -> Integer
sumNumbers' l = sum $ map read $ filter (not . null) $ go l []
  where go :: String -> [String] -> [String]
        go [] acc = acc
        go (h : t) [] | isDigit h = go t [[h]]
                      | otherwise = go t []
        go (h : t) (accH : accT) | isDigit h = go t ((h : accH) : accT)
                                 | otherwise = go t ([] : accH : accT)

wordCount :: String -> String
wordCount s =
  "Number of lines: " ++ numOfLines ++
  "\nNumber of empty lines: " ++ numOfNonemptyLines ++
  "\nNumber of words: " ++ numOfWords ++
  "\nNumber of unique words: " ++ numOfUniqueWords ++
  "\nNumber of words followed by themselves:" ++ numOfRepeatedWords ++
  "\nLength of the longest line: " ++ longestLineLength
    where lin = lines s
          numOfLines         = show $ length lin
          numOfNonemptyLines = show $ length $ filter (not . null) lin
          numOfWords         = show $ length $ words s
          numOfUniqueWords   = show $ length $ nub $ words s
          numOfRepeatedWords = show $ length $ filter (\g -> length g > 1) $ group $ words s
          longestLineLength  = show $ length $ longestText lin

{-# LANGUAGE OverloadedStrings #-}
module Drawing
  (
    Picture
  , blank
  , (&)
  , char
  , text
  , translated
  , drawingOf
  ) where

import           Data.Maybe

type DrawFun = Integer -> Integer -> Char

type Picture = DrawFun -> DrawFun

blank :: Picture
blank = id

(&) :: Picture -> Picture -> Picture
(&) = (.)

char :: Char -> Picture
char c _ 0 0 = c
char _ f x y = f x y

getElem :: [a] -> Int -> Maybe a
getElem l n | n < length l && n >= 0 = Just $ l !! n
            | otherwise              = Nothing

text' :: String -> Picture
text' s f x 0 = let m = getElem s $ fromIntegral x
               in fromMaybe (f x 0) m
text' _ f x y = f x y

text :: String -> Picture
text s = translated (-(fromIntegral (length s) `div` 2)) 0 $ text' s

translated :: Integer -> Integer -> Picture -> Picture
translated dx dy p f x y = p (\x1 y1 -> f (x1 + dx) (y1 + dy)) (x - dx) (y - dy)

drawingOf :: Picture -> String
drawingOf p = unlines [ [f x y | x <- [-10..10] ] | y <- [10,9 .. -10] ]
  where f = p (\_ _ -> ' ')

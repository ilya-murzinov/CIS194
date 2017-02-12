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
  , fib
  , fibs1
  , fibs2
  , nats
  , ruler
  , Supply
  , get
  , runSupply
  , labelTree
  , Functor
  , Applicative
  , Monad
  ) where

import           Data.Char
import           Data.List
import           Data.Ord

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = init (show (take 20 (streamToList s))) ++ "..."

data Supply s a = S (Stream s -> (a, Stream s))

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pureSupply
  (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

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

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0,1..10]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f x = Cons x (streamIterate f (f x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S go
  where go xs = (f a, xs')
                where (a, xs') = t xs

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S t1) (S t2) = S go
  where go xs = (f a b, xs'')
                where (a, xs') = t1 xs
                      (b, xs'') = t2 xs'

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S t1) f = S go
  where go xs = (b, xs'')
                where (a, xs')  = t1 xs
                      S t2      = f a
                      (b, xs'') = t2 xs'

runSupply :: Stream s -> Supply s a -> a
runSupply s (S t) = fst $ t s

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node t1 t2) = Node <$> go t1 <*> go t2
    go (Leaf _) = Leaf <$> get

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA c) where
  fmap f (Con1 c a) = Con1 c (f a)
  fmap f (Con2 l) = Con2 (map (fmap (f .)) l)

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor g => Functor (ComplicatedB f g a) where
  fmap _ (Con3 fa) = Con3 fa
  fmap f (Con4 fb) = Con4 $ f <$> fb
  fmap f (Con5 g) = Con5 (fmap (fmap (map f)) g)

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' = fmap (\x -> (x,x))

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' xs = (\x y -> (x,y)) <$> xs <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' xs = (\x _ -> (x,x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (\x y -> (x,y)) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> x + y + 2) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Applicative f => f Integer -> f (Integer,Integer)
func6' = fmap (\x -> if x > 0 then (x, 0) else (0, x))

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func7' :: Applicative f => f Integer -> f (Integer,Integer)
func7' = func6'

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = fmap (+x) xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap $ \x -> x * x + 10

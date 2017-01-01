{-# LANGUAGE OverloadedStrings #-}
module Sokoban
  (
    List (..)
  , Tile (..)
  , Coord (..)
  , Direction (..)
  , Maze (..)
  , State (..)
  , mapList
  , combine
  , isLast
  , isWon
  , combine21times
  , noBoxMaze
  , mazeWithBoxes
  , validMazes
  , getMaze
  , loadLevel
  , tryMove
  ) where

data List a = Empty | Entry a (List a)

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

data Coord = C Integer Integer

data Direction = R | U | L | D deriving Eq

data Maze = Maze Coord (Coord -> Tile)

data State = S Coord Direction (List Coord) Integer deriving Eq

instance Eq Coord where
  (==) (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

instance Eq a => Eq (List a) where
  (==) Empty Empty                 = True
  (==) Empty _                     = False
  (==) _ Empty                     = False
  (==) (Entry h1 t1) (Entry h2 t2) = h1 == h2 && t1 == t2

singleton :: a -> List a
singleton a = Entry a Empty

mapList :: (a -> b) -> List a -> List b
mapList _ Empty        = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

appendList :: List a -> List a -> List a
appendList Empty second       = second
appendList (Entry h t) second = Entry h $ appendList t second

elemList :: Eq a => a -> List a -> Bool
elemList _ Empty = False
elemList e (Entry h t)
  | h == e    = True
  | otherwise = elemList e t

listLength :: List a -> Integer
listLength Empty       = 0
listLength (Entry _ t) = 1 + listLength t

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList f (Entry h t)
  | f h       = Entry h $ filterList f t
  | otherwise = filterList f t

nth :: List a -> Integer -> a
nth Empty _       = undefined
nth (Entry h _) 0 = h
nth (Entry _ t) n = nth t (n-1)

combine :: a -> (a -> a -> a) -> List a -> a
combine acc _ Empty            = acc
combine acc f (Entry p ps)     = combine (f acc p) f ps

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed e adj isOk = go (adj e) (singleton e)
  where go Empty _ = True
        go (Entry h t) visited
          | elemList h visited = go t visited
          | not $ isOk h       = False
          | otherwise          = go (appendList t $ adj h) (Entry h visited)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

validMazes :: List Maze -> List Maze
validMazes = filterList isClosed

getMaze :: Integer -> List Maze ->  (Coord -> Tile)
getMaze n mazes = case nth mazes n of (Maze _ m) -> m

isLast :: Integer -> List Maze -> Bool
isLast n mazes = listLength mazes == n + 1

getInit :: Integer -> List Maze -> Coord
getInit n mazes = case nth mazes n of (Maze c _) -> c

noBoxMaze :: (Coord -> Tile) -> Coord -> Tile
noBoxMaze maze c
  | isBox $ maze c = Ground
  | otherwise      = maze c

mazeWithBoxes :: (Coord -> Tile) -> List Coord -> Coord -> Tile
mazeWithBoxes maze Empty c = noBoxMaze maze c
mazeWithBoxes maze (Entry h t) c
  | h == c = Box
  | otherwise   = mazeWithBoxes maze t c

isClosed :: Maze -> Bool
isClosed (Maze c m) = available (m c) &&
  isGraphClosed c (adjacent m) (\coord -> m coord /= Blank)

adjacent :: (Coord -> Tile) -> Coord -> List Coord
adjacent m c =
  filterList (\coord -> m coord /= Wall) (allDirections c)
  where allDirections coord = mapList (`adjacentCoord` coord) $
                              Entry R $ Entry D $ Entry L $ Entry U Empty

initialCoords :: (Coord -> Tile) -> (Tile -> Bool) -> List Coord
initialCoords maze f = combine21times Empty appendList
  (combine21times Empty appendList . go)
    where go :: Integer -> Integer -> List Coord
          go x y
            | f $ maze c = Entry c Empty
            | otherwise = Empty
            where c = C x y

combine21times :: out -> (out -> out -> out) -> (Integer -> out) -> out
combine21times e comb something = go (-10)
  where
    go 11 = e
    go n  = comb (something n) (go (n+1))

isBox :: Tile -> Bool
isBox Box = True
isBox _   = False

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _       = False

initialBoxes :: (Coord -> Tile) -> List Coord
initialBoxes maze = initialCoords maze isBox

storages :: (Coord -> Tile) -> List Coord
storages maze = initialCoords maze isStorage

loadLevel :: Integer -> List Maze -> State
loadLevel n mazes = S (getInit n mazes) R (initialBoxes $ getMaze n mazes) n

available :: Tile -> Bool
available Ground  = True
available Storage = True
available _       = False

tryMove :: (Coord -> Tile) -> Coord -> Direction -> List Coord -> Integer -> State
tryMove maze from d boxes level
  | available $ mazeWithBoxes maze boxes to           = S to d boxes level
  | isBox (mazeWithBoxes maze boxes to)
      && available (mazeWithBoxes maze boxes boxTo) =
        S to d (moveBox to boxTo boxes) level
  | otherwise                                         = S from d boxes level
    where to    = adjacentCoord d from
          boxTo = adjacentCoord d to

moveBox :: Coord -> Coord -> List Coord -> List Coord
moveBox _ _ Empty = Empty
moveBox from to (Entry h t)
  | from == h = Entry to t
  | otherwise = Entry h (moveBox from to t)

isWon :: (Coord -> Tile) -> State -> Bool
isWon maze (S _ _ boxes _) = boxes == storages maze

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

singleton :: a -> List a
singleton a = Entry a Empty

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

appendList :: List a -> List a -> List a
appendList Empty second       = second
appendList (Entry h t) second = Entry h $ appendList t second

combine :: List Picture -> Picture
combine Empty        = blank
combine (Entry p ps) = p & combine ps

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

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed e adj isOk = go (adj e) (singleton e)
  where go Empty _ = True
        go (Entry h t) visited
          | elemList h visited = go t visited
          | not $ isOk h       = False
          | otherwise          = go (appendList t $ adj h) (Entry h visited)

-- Coordinates

data Coord = C Integer Integer

data Direction = R | U | L | D deriving Eq

instance Eq Coord where
  (==) (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

instance Eq a => Eq (List a) where
  (==) Empty Empty                 = True
  (==) Empty _                     = False
  (==) _ Empty                     = False
  (==) (Entry h1 t1) (Entry h2 t2) = h1 == h2 && t1 == t2

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

-- The maze

data Maze = Maze Coord (Coord -> Tile)

validMazes :: List Maze
validMazes = filterList isClosed extraMazes

getMaze :: Integer -> (Coord -> Tile)
getMaze n = case nth validMazes n of (Maze c m) -> m

isLast :: Integer -> Bool
isLast n = listLength validMazes == n + 1

getInit :: Integer -> Coord
getInit n = case nth validMazes n of (Maze c m) -> c

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

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
isClosed (Maze c m) = (available $ m c) &&
  isGraphClosed c (adjacent m) (\coord -> m coord /= Blank)

adjacent :: (Coord -> Tile) -> Coord -> List Coord
adjacent m c =
  filterList (\coord -> m coord /= Wall) (allDirections c)
  where allDirections c = mapList (\d -> adjacentCoord d c) $
                          Entry R $ Entry D $ Entry L $ Entry U Empty

-- The state

data State = S Coord Direction (List Coord) Integer deriving Eq

initialCoords :: (Coord -> Tile) -> (Tile -> Bool) -> List Coord
initialCoords maze f = combine21times Empty appendList
  (\r -> combine21times Empty appendList (\c -> go r c))
    where go :: Integer -> Integer -> List Coord
          go x y
            | f $ maze c = Entry c Empty
            | otherwise = Empty
            where c = C x y

isBox :: Tile -> Bool
isBox Box = True
isBox _ = False

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _       = False

initialBoxes :: (Coord -> Tile) -> List Coord
initialBoxes maze = initialCoords maze isBox

storages :: (Coord -> Tile) -> List Coord
storages maze = initialCoords maze isStorage

loadLevel :: Integer -> State
loadLevel n = S (getInit n) R (initialBoxes $ getMaze n) n

-- Event handling

available :: Tile -> Bool
available Ground = True
available Storage = True
available _ = False

tryMove :: (Coord -> Tile) -> Coord -> Direction -> List Coord -> Integer -> State
tryMove maze from d boxes level
  | available $ mazeWithBoxes maze boxes to           = S to d boxes level
  | (isBox $ mazeWithBoxes maze boxes to)
      && (available $ mazeWithBoxes maze boxes boxTo) =
        S to d (moveBox to boxTo boxes) level
  | otherwise                                         = S from d boxes level
    where to    = adjacentCoord d from
          boxTo = adjacentCoord d to

moveBox :: Coord -> Coord -> List Coord -> List Coord
moveBox _ _ Empty = Empty
moveBox from to (Entry h t)
  | from == h = Entry to t
  | otherwise = Entry h (moveBox from to t)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S c d boxes level)
    | isWon maze state && isLast level = state
    | isWon maze state && key == " "   = loadLevel (level + 1)
    | key == "Right"                   = tryMove maze c R boxes level
    | key == "Up"                      = tryMove maze c U boxes level
    | key == "Left"                    = tryMove maze c L boxes level
    | key == "Down"                    = tryMove maze c D boxes level
    | otherwise                        = state
    where state = S c d boxes level
          maze  = getMaze level
handleEvent _ s      = s

isWon :: (Coord -> Tile) -> State -> Bool
isWon maze (S _ _ boxes _) = boxes == storages maze

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze maze = combine21times blank (&)
  (\r -> combine21times blank (&) (\c -> drawTileAt maze (C r c)))

combine21times :: out -> (out -> out -> out) -> (Integer -> out) -> out
combine21times e comb something = go (-10)
  where
    go 11 = e
    go n  = comb (something n) (go (n+1))

drawTileAt :: (Coord -> Tile) -> Coord -> Picture
drawTileAt maze c = atCoord c (drawTile (noBoxMaze maze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

triangle :: Picture
triangle = translated (-0.5) (-0.5) $
  colored red $ path[(0.2,0.2), (0.2,0.8)]
    & path[(0.2,0.8), (0.8,0.5)]
    & path[(0.2,0.2), (0.8,0.5)]

player :: Direction -> Picture
player R = triangle
player U = rotated (pi/2) triangle
player L = rotated pi triangle
player D = rotated (-pi/2) triangle

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

esc :: Picture
esc = translated 0 (-9) $ text "Press 'Esc' to reset"

winScreen :: Picture
winScreen = scaled 3 3 (text "You won!") &
            esc &
            (translated 0 (-6) $ text "Press 'Space' for next level")

allDoneScreen :: Picture
allDoneScreen = scaled 3 3 (text "All done!")

drawState :: State -> Picture
drawState (S c d boxes level)
  | isWon maze (S c d boxes level) && isLast level = allDoneScreen & esc
  | isWon maze (S c d boxes level)                 = winScreen & base
  | otherwise                                      = esc & base
  where maze = getMaze level
        base = (atCoord c $ player d) &
               (pictureOfBoxes boxes) &
               pictureOfMaze maze

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction (loadLevel 0) (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!") &
  translated 0 (-5) $ text "Press 'Space' to play"

data SSState world = StartScreen | Running world

instance Eq s => Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

data WithUndo a = WithUndo a (List a)

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = WithUndo state0 Empty

    step' t (WithUndo s stack) = WithUndo (step t s) stack

    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo s' (Entry s stack)
      where s' = handle e s

    draw' (WithUndo s _) = draw s

pictureOfBools :: List Bool -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = listLength xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ Empty = blank
        go i (Entry b bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

pictureOfBool :: Bool ->  Picture
pictureOfBool True  =  colored green (solidCircle 0.4)
pictureOfBool False = colored red   (solidCircle 0.4)


-- The main function

exercise3 :: Picture
exercise3 = pictureOfBools $ mapList isClosed extraMazes

main :: IO ()
main = runInteraction $ resetable $ withUndo $ withStartScreen sokoban

--------------------------------------------------------------------------------

mazes :: List Maze
{-| mazes = Entry (Maze (C 1 1)       maze0) $
            Entry (Maze (C 1 1)       maze0) $ Empty |-}
mazes =
  Entry (Maze (C 1 1)       maze0) $
  Entry (Maze (C 1 1)       maze9) $
  Entry (Maze (C 0 0)       maze8) $
  Entry (Maze (C (-3) 3)    maze7) $
  Entry (Maze (C (-2) 4)    maze6) $
  Entry (Maze (C 0 1)       maze5) $
  Entry (Maze (C 1 (-3))    maze4) $
  Entry (Maze (C (-4) 3)    maze3) $
  Entry (Maze (C 0 1)       maze1) $
  Empty

extraMazes :: List Maze
extraMazes =
  Entry (Maze (C 1 (-3))    maze4') $
  Entry (Maze (C 1 (-3))    maze4'') $
  Entry (Maze (C 1 1)       maze9') $
  mazes

maze0 :: Coord -> Tile
maze0 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 0 && y == 0         = Storage
  | x == 1 && y == 0         = Box
  | otherwise                = Ground

maze1 :: Coord -> Tile
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall

maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall

maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall

maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall

maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall

maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall

maze3 _ = Blank

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

maze5 :: Coord -> Tile
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Coord -> Tile
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Coord -> Tile
maze7 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground

maze8 :: Coord -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10    = Blank
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground

maze9 :: Coord -> Tile
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c

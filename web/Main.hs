{-# LANGUAGE OverloadedStrings #-}
import           CodeWorld
import           Sokoban
import           Mazes

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
  (\r -> combine21times blank (&) (drawTileAt maze . C r))

drawTileAt :: (Coord -> Tile) -> Coord -> Picture
drawTileAt maze c = atCoord c (drawTile (noBoxMaze maze c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromIntegral x) (fromIntegral y)

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
pictureOfBoxes cs = combine blank (&) $
  mapList (\c -> atCoord c (drawTile Box)) cs

esc :: Picture
esc = translated 0 (-9) $ text "Press 'Esc' to reset"

winScreen :: Picture
winScreen = scaled 3 3 (text "You won!") &
            esc &
            translated 0 (-6) (text "Press 'Space' for next level")

allDoneScreen :: Picture
allDoneScreen = scaled 3 3 (text "All done!")

mazes :: List Maze
mazes = validMazes extraMazes

drawState :: State -> Picture
drawState (S c d boxes level)
  | isWon maze (S c d boxes level) && isLast level mazes = allDoneScreen & esc
  | isWon maze (S c d boxes level)                       = winScreen & base
  | otherwise                                            = esc & base
  where maze = getMaze level mazes
        base = atCoord c (player d) &
               pictureOfBoxes boxes &
               pictureOfMaze maze

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S c d boxes level)
   | isWon maze state && isLast level mazes = state
   | isWon maze state && key == " "         = loadLevel (level + 1) mazes
   | key == "Right"                         = tryMove maze c R boxes level
   | key == "Up"                            = tryMove maze c U boxes level
   | key == "Left"                          = tryMove maze c L boxes level
   | key == "Down"                          = tryMove maze c D boxes level
   | otherwise                              = state
   where state = S c d boxes level
         maze  = getMaze level mazes
handleEvent _ s      = s

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction (loadLevel 0 mazes) (\_ c -> c) handleEvent drawState

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
        handle' e s              = handle e s

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
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

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

main :: IO ()
main = runInteraction $ resetable $ withUndo $ withStartScreen sokoban

{-# LANGUAGE OverloadedStrings #-}
import           Sokoban
import           Mazes
import           Drawing
import           System.IO

data Interaction world = Interaction
  world
  (Char -> world -> world)
  (world -> String)

wall, ground, storage, box :: Char
wall    = '#'
ground  = ' '
storage = '.'
box     = 'O'

drawTile :: Tile -> Char
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = ground

player :: Direction -> Char
player R = '>'
player U = '^'
player L = '<'
player D = 'V'

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromIntegral x) (fromIntegral y)

drawTileAt :: (Coord -> Tile) -> Coord -> Picture
drawTileAt maze c = atCoord c (char $ drawTile (noBoxMaze maze c))

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze maze = combine21times blank (&)
  (\r -> combine21times blank (&) (drawTileAt maze . C r))

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine blank (&) $
  mapList (\c -> atCoord c (char $ drawTile Box)) cs

esc :: Picture
esc = translated 0 (-9) $ text "Press 'Esc' to reset"

winScreen :: Picture
winScreen = text "You won!" &
            esc &
            translated 0 (-6) (text "Press 'Space'") &
            translated 0 (-7) (text "for next level")

allDoneScreen :: Picture
allDoneScreen = text "All done!"

drawState :: State -> Picture
drawState (S c d boxes level)
  | isWon maze (S c d boxes level) && isLast level mazes = allDoneScreen & esc
  | isWon maze (S c d boxes level)                       = winScreen & base
  | otherwise                                            = esc & base
  where maze = getMaze level mazes
        base = atCoord c (char $ player d) &
               pictureOfBoxes boxes &
               pictureOfMaze maze

handleEvent :: Char -> State -> State
handleEvent key (S c d boxes level)
  | isWon maze state && isLast level mazes = state
  | isWon maze state && key == ' '         = loadLevel (level + 1) mazes
  | isWon maze state                       = state
  | key == 'd'                             = tryMove maze c R boxes level
  | key == 'w'                             = tryMove maze c U boxes level
  | key == 'a'                             = tryMove maze c L boxes level
  | key == 's'                             = tryMove maze c D boxes level
  | otherwise                              = state
  where state = S c d boxes level
        maze  = getMaze level mazes

mazes :: List Maze
mazes = validMazes extraMazes

sokoban :: Interaction State
sokoban = Interaction
  (loadLevel 0 mazes)
  handleEvent $
  drawingOf . drawState

blankScreen :: IO ()
blankScreen = putStr "\ESCc"

runInteraction :: Interaction State -> IO ()
runInteraction (Interaction (S c d boxes level) handle draw) =
  do hSetBuffering stdin NoBuffering
     blankScreen
     putStr $ draw state
     key <- getChar
     let s = handle key state
     runInteraction $ Interaction s handle draw
       where state = S c d boxes level

main :: IO ()
main = runInteraction sokoban

# sokoban-haskell

## Exercise 3: Sokoban tiles

Over the next few homeworks, you will implement a game of Sokoban. The rules are simple (quoted from https://en.wikipedia.org/wiki/Sokoban, which also contains a nice animation that you can look at):

The game is played on a board of squares, where each square is a floor or a wall. Some floor squares contain boxes, and some floor squares are marked as storage locations.

The player is confined to the board, and may move horizontally or vertically onto empty squares (never through walls or boxes). The player can also move into a box, which pushes it into the square beyond. Boxes may not be pushed into other boxes or walls, and they cannot be pulled. The puzzle is solved when all boxes are at storage locations.

We do some preparations this week. In particular, we need the different squares that may occur:

Walls

Ground, i.e. empty spaces

Ground marked as storage

Boxes

After this exercise, we will have the necessary code to draw a sokoban level.

Create a functions wall, ground, storage and box of type Picture, which draw the corresponding thing, with a width and height of 1 and positioned at the center of the picture.

The example picture below is very much on the dull side. Make it prettier! You can search for screenshots of the real game for inspiration.

Create a function drawTile :: Integer -> Picture that draws a tile according to the given number2, according to the above list. If the argument is not one of these four numbers, it should not draw anything (but should also not crash).

A maze can be represented as a function with the type
Integer -> Integer -> Integer, which, given two coordinates, returns the kind of tile to be present there.

For now, we have one such maze, with the creative name maze:

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
Define a picture

pictureOfMaze :: Picture
which will, for every coordinate (x,y)(x,y) with x,y∈−10,…,10x,y∈−10,…,10, uses the maze above to determine what tile to place there, uses drawTile to draw that tile, translated into the right spot.

Of course, do not hard-code 441 invocations of drawTile into your program! Instead, use recursion to traverse the positions in the grid. You will likely need one recursive function to draw one row after another, which in turn calls a second recursive function, which then draw each element in that row.

Define exercise3 to be drawing of pictureOfMaze.

##Exercise 1: The small guy (or girl) moves

We hope to have a complete game by next week, so let us work towards that.

Create a value player :: Picture that draws your figure.

Create a value exercise1 :: IO () that calls interactionOf with suitable arguments that:

the player is drawn on top of the maze,
it starts in a position where there is ground (you can hard-code that position),
the cursor keys move the figure around (while the maze stays in a fixed position),
the player moves only on tiles of type Ground and Storage. Trying to move it into any other position will simply leave it in place.
It might yield nicer code to change the type of maze to Coord -> Tile. If you find that, do not hesitate to make that change.


## Exercise 2: Look the right way!

This is an extension of exercise 1. We want to figure to look the way it is going. So write a function player2 :: Direction -> Picture that draws the figure in four variants.

Then extend the code from exercise1 so that after the player has tried to move in some direction, it looks that way. If you can re-use functions and types from exercise1, do so, but if you have to change them, rename them, e.g. by appending a 2 to the name.

Hint: Think about types first (e.g of your state), and then about the implementation.

The resulting program should be called exercise2.


##Exercise 3: Reset!

It would be nice to be able to start a game from the beginning. This is generally useful functionality, no matter what the game, so let us implement it generally.

You will write a function

resetableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
which has the same type as interactionOf.

This function will behave almost the same as interactionOf (which it internally uses, of course), but when Esc is pressed, this event is not passed on, but rather the state of the program is reset to the given initial state.

Style hint: An idiomatic definition of resetableInteractionOf does not require any other top-level definitions, but likely local functions and/or lambda expressions.

Let exercise3 be like exercise2, but using resetableInteractionOf instead of interactionOf.


## Exercise 4: New levels

Create a new maze. It should

fit within the screen (i.e. use coordinates from -10 to 10),
it should be connected (i.e. starting on a ground tile, and disregarding boxes, the player should be able to reach all ground tiles),
it should be closed (i.e. the player should not be able to reach blank tiles), and
it should be solvable (recollect the rules if necessary).
Use this maze in your exercises above, so that the TA does not get bored while grading.

Also, we will be collecting these mazes and provide them to you so that you can build a sokoban game with different levels next week.

## Step 1: The state

Define a data type State that capture the state of the game. It needs to store these bits of information:

The current position of the player.
The direction the player is facing.
The current positions of all boxes, as a List.
## Step 2: The initial state

Define a value initialState :: State for the initial state:

Manually define a sensible position for the player (i.e. on some Ground tile).
Use an arbitrary position.
Find where the boxes are.
The latter is a bit tricky: The information is there (in the definition of maze), but not very accessible. Do not just write down the list of coordinates by hand! Instead, define a value initialBoxes :: List Coord that is calcuated from looking at each coordinate (going from -10 to 10, as usual), and adding that coordinate to the list if there is a box.

There are two ways of doing that. Pick one (but try to understand both):

Define a function

appendList :: List a -> List a -> List a
which appends two lists. Then you can implement initialBoxes similar to pictureOfMaze, using appendList instead of (&).

This is called folding appendList over the set of coordinate.

Alternatively, in your recursions that traverse the coordinate space, pass a list down as a parameter. Initially, this list is Empty. If the current coordinate is not a Box, you simply pass it on. If the current coordinate is a box, you add an Entry to the list, and pass the extended list on.

The helper function could possibly have type

go :: Integer -> Integer -> List Coord -> List Coord
Such a parameter is called an accumulating parameter.

You can test your initialBoxes value using

main = drawingOf (pictureOfBoxes initialBoxes)
## Step 3: Many mazes

The maze as given has boxes in the initial position. That is no good in the long run. Therefore, you need to define two more mazes:

Define

noBoxMaze :: Coord -> Maze
which behaves like maze with the exception that where maze would return a Box, this returns Ground.

Use this in pictureOfMaze instead of maze. You can test this using

main = drawingOf pictureOfMaze
Define

mazeWithBoxes :: List Coord -> (Coord -> Maze)
which behaves like noBoxMaze for every coordinate that is not in the list, but returns Box if queried for a coordinate that is in the given list of coordinates.

It will be useful (also below) to define a function

eqCoord :: Coord -> Coord -> Bool
that checks if two coordinates are the same.

Note that mazeWithBoxes, partially applied to one argument (the list with the curren positions of the boxes), has the same type as maze.

## Step 4: Draw the state

Implement

draw :: State -> Picture
The picture consists of three elements:

The player, at the current position (use player and atCoord).
The boxes in their current positions (use pictureOfBoxes).
The pictureOfMaze (which uses noBoxMaze internally).
## Step 5: Handling event

Implement

handleEvent :: Event -> State -> State
React to the arrow keys only. such an event can either succeed or fail.

It succeeds if the tile moved to is either Ground or Storage or Box. If it is Box, the next tile in that direction has to be Ground or Storage. It fails otherwise.

If the move succeeds, update the state with the new position of the player, the direction he walked to, and the updated position of the boxes.

Hint (you can do it differently if you want): To update the position of the boxes, define a function

moveFromTo :: Coord -> Coord -> (Coord -> Coord)
that takes a position to move from, a position to move to, and the coordinate to adjust. If the first parameter matches the third, return the second, otherwise the third. You can use this function with mapList to update the whole list of coordinates. This conveniently does the right thing (i.e. nothing) if no box is pushed, and does the right thing with boxes that are not pushed. Do not worry about efficiency here.

If the move fails, update only the direction the player.

## Step 6: Putting it all together

Define an Interaction based on the functions you defined in the prevoius steps. Wrap it in resetable and withStartScreen, so that pressing the escape key returns to the start screen.

It should now behave roughly like this:


## Step 7: Winning

One bit is missing: If the game has been won, you need to say so, and futher interaction should stop.

Define a function

isWon :: State -> Bool
which is True if the game is won. It is won if every box is on a Storage tile. To that end, define two helper functions:

haskell isOnStorage :: Coord -> Bool that returns true if the given coordinate is a Storage field.

haskell allList :: List Bool -> Bool which returns true if all entries in the list are True, and False otherwise. If given an empty list it should also return True.

You can implement isWon using these two functions and mapList.

Use isWon in two places:

In draw, if the game is won, write You won! or something similar across the drawn picture of the game.
In handleEvent, if the game is won, ignore all events (i.e. return the state unaltered)
The final game should now behave like this:

## Exercise 0: Import the list of mazes

We have collected your submitted mazes from last week. You can download the code of the mazes. It contains a list of mazes (mazes) and a longer list including broken mazes (extraMazes). Paste them into your file, at the very end.

Because the starting position is relevant, we added a data type to go along with the maze:

data Maze = Maze Coord (Coord -> Tile) 
mazes :: List Maze
mazes = …
extraMazes :: List Maze
extraMazes = …
## Exercise 1: More polymorphic list function

Implement these generally useful functions:

elemList :: Eq a => a -> List a -> Bool
appendList :: List a -> List a -> List a
listLength :: List a -> Integer
filterList :: (a -> Bool) -> List a -> List a
nth :: List a -> Integer -> a
These should do what their name and types imply:

elemList x xs is True if and only if at least one entry in xs equals to x.
appendList xs ys should be the list containing the entries of xs followed by those of ys, in that order.
listLength xs should be the number of entries in xs.
filterList p xs should be the list containing those entries x of xs for which p x is true.
nths xs n extracts the nnth entry of the list (start counting with 1). If nn is too large, you may abort the program (by writing error "list too short", which is an expression that can be used at any type). This is not good style, but shall do for now.
## Exercise 2: Graph search

(Read exercise 3 first, to have understand why this is an interesting function.)

The algorithm you have to implement below can be phrased very generally, and we want it to be general. So implement a function

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
so that in a call isGraphClosed initial adjacent isOk, where the parameters are

initial, an initial node,
adjacent, a function that for every node lists all walkable adjacent nodes and
isOk, which checks if the node is ok to have in the graph,
the function returns True if all reachable nodes are “ok” and False otherwise.

Note that the graph described by adjacent can have circles, and you do not want your program to keep running in circles. So you will have to remember what nodes you have already visted.

The algorithm follows quite naturally from handling the various cases in a local helper function go that takes two arguments, namely a list of seen nodes and a list of nodes that need to be handled. If the latter list is empty, you are done. If it is not empty, look at the first entry. Ignore it if you have seen it before. Otherwise, if it is not ok, you are also donw. Otherwise, add its adjacent elements to the list of nodes to look ak.

You might find it helpful to define a list allDirections :: List Direction and use mapList and filterList when implementing adjacent.

## Exercise 3: Check closedness of mazes

Write a function

isClosed :: Maze -> Bool
that checks whether the maze is closed. A maze is closed if

the starting position is either Ground or Storage and
every reachable tile is either Ground, Storage or Box.
Use isGraphClosed to do the second check. Implement adjacent so that isGraphClosed walks everywhere where there is not a Wall (including Blank). Implement isOk so that Blank tiles are not ok.

With the following function you can visualize a list of booleans:

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

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)
Let exercise3 :: IO () be the visualization of isClosed applied to every element of extraMazes. Obviously, mapList wants to be used here.


## Exercise 4: Multi-Level Sokoban

Extend your game from last week (or the code from the lecture) to implement multi-level sokoban.

Extend the State with a field of type Integer, to indicate the current level (start counting at 1).
The initial state should start with level 1. The initial coordinate is obtained read from the entry in maze.
Your handle and draw functions will now need to take an additional argument, the current maze, of type Coord -> Tile, instead of refering to a top-level maze function. Any helper functions (e.g. noBoxMaze) will also have to take this as an argument. This requires many, but straight-forward changes to the code: You can mostly, without much thinking:

Check the compier errors for an affected function, say foo.
Add (Coord -> Tile) -> to the front of foo’s type signature, .
Add a new first parameter maze to foo
Everywhere where foo is called, add maze as an argument.
Repeat.
To get the current maze, use nth from exercise 1. Of course, make sure you never use nth with a too-short list. A variant nthMaze :: Integer -> (Coord -> Tile) that gets the maze component of the corresponding entry in mazes will also be handy whenever you have the State, but need a maze :: Coord -> Tile.

If the level is solved and the current level is not the last (use listLength from above) the space bar should load the next level.

There is some code to be shared with the calculation of the initial state! Maybe the same function loadLevel :: Integer -> State can be used in both situations.
If the level is solved and the current leve is the last, show a differnt message (e.g. “All done” instead of “You won”).

Let exercise4 :: IO () be this interaction, wrapped in withUndo, withStartScreen and resetable.

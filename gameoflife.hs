import Data.List

-- Clears Screen
clearSc :: IO ()
clearSc = putStr "\ESC[2J"

type Point = (Int, Int)
type Board = [Point]

-- Add the respective x and y coordinates of two points and returns a point
addPoints :: Point -> Point -> Point
addPoints (a, b) (c, d) = (a + c, b + d)

-- Return the number of live neighbors of a point as an integer
numLiveNeighbors :: Point -> Board -> Int
numLiveNeighbors p b =
  length (intersect [addPoints p (x, y) | x <- [-1..1], y <- [-1..1]] b)

-- Cursor movements escape sequences http://tldp.org/HOWTO/Bash-Prompt-HOWTO/x361.html
-- Move cursor to desired point
-- Print string (representing live point)
writeAt :: Point -> String -> IO ()
writeAt (x, y) s = do
  putStr ("\ESC[" ++ show y ++ ";" ++ show (x * 2) ++ "H")
  putStr s

-- Print all live cells in board without returning anything
-- sequence will return values, sequence_ will not
printBoard :: Board -> IO ()
printBoard b = do
  clearSc
  sequence_ [writeAt p "[]" | p <- b]
  putStrLn ""

newPoints :: Board -> Board
newPoints b =
  let
    width = (+10) $ maximum $ map fst b
    height = (+10) $ maximum $ map snd b
  in
    [(x, y) | x <- [-width..width], y <- [-height..height], not $ (x, y) `elem` b, numLiveNeighbors (x, y) b == 3]

survivingPoints :: Board -> Board
survivingPoints b = filter (\p -> (numLiveNeighbors p b - 1) `elem` [2,3] ) b

nextBoard :: Board -> Board
nextBoard b = (newPoints b) ++ (survivingPoints b)

game :: Board -> IO ()
game b = do
  cmd <- getLine
  if cmd == "" && length b > 0 then do
    printBoard b
    game $ nextBoard b
  else
    return ()

-- Flyer
board_flyer :: Board
board_flyer = [(2, 2), (4, 2), (3, 3), (4, 3), (3, 4)]

main :: IO ()
main = do
  game board_flyer

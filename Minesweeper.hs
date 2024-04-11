import System.Random
import Data.List

-- game types, GameState can be Playing, Won or Lost
-- Tile can be Mine, FlaggedMine, Empty with the number of mines around it, Flagged with the number of mines around it,
-- or Revealed with the number of mines around it
-- Board is an array of arrays of Tiles (2D array)
-- Coordinate is a tuple of two integers (x, y)
data GameState = Playing | Won | Lost deriving (Show, Eq)
data Tile = Mine | FlaggedMine | Empty Int | Flagged Int | Revealed Int deriving (Show, Eq)

type Board = [[Tile]]
type Coordinate = (Int, Int)

-- generate a board with random mines
-- function takes in width of the board, height of the board, number of mines and a random generator
-- returns a Board with mines randomly placed on the board
generateBoard :: Int -> Int -> Int -> StdGen -> Board
generateBoard width height mines gen = 
  let
    -- generate a list of random mine positions
    -- takes mines amount of random numbers between 0 and width * height - 1, duplicates removed (nub)
    minePositions = take mines $ nub $ randomRs (0, width * height - 1) gen

    -- generate a list of tiles, if the index is in minePositions, the tile is a Mine, else it is Empty
    tiles = [if i `elem` minePositions then Mine else Empty 0 | i <- [0..width * height - 1]]

    -- generate a list of rows
    -- batches the tiles into rows of given width
    rows = [take width $ drop (i * width) tiles | i <- [0..height - 1]]
  in
    rows


-- calculate numbers of mines around each tile
-- function takes in Board and returns a new Board with the number of mines around each tile
-- this function is called in main right after generateBoard
calculateNumbers :: Board -> Board
calculateNumbers board =
  let
    width = length $ head board -- width is the length of the first row on Board
    height = length board -- height is the length of the Board

    -- get the number of mines around a tile
    -- function takes in a Board and a Coordinate and returns the number of mines around the tile
    getNumber :: Board -> Coordinate -> Int
    getNumber board (x, y) =
      let
        -- dx, dy = -1, 0, 1, guarding that the neighbor coordinates are not 0,0 which is currently investigated tile
        -- also guarding that the neighbor coordinates are within the board boundaries
        -- output of this row is a list of coordinates of neighboring tiles, for example (2,2) -> [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]
        neighbours = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0, x + dx >= 0, x + dx < height, y + dy >= 0, y + dy < width]
        tiles = map (\(x, y) -> board !! x !! y) neighbours -- map the neighboring coordinates to the tiles
      in
        length $ filter (\tile -> tile == Mine || tile == FlaggedMine) tiles -- count the number of Mines and FlaggedMines in the neighboring tiles
    
    -- go through each tile that is Empty on the board and calculate the number of mines around it
    newBoard = [[case tile of
      Empty _ -> Empty (getNumber board (x, y))
      _ -> tile | (y, tile) <- zip [0..] row] | (x, row) <- zip [0..] board]
  in
    newBoard


-- flag a tile method
-- function takes in a Board and a Coordinate and returns a new Board with the tile flagged or unflagged depending on its current state
flagTile :: Board -> Coordinate -> Board
flagTile board (x, y) =
  let
    tile = board !! x !! y -- get the tile from board
    newTile = case tile of
      Flagged n -> Empty n -- if the tile is Flagged, unflag it
      FlaggedMine -> Mine -- if the tile is FlaggedMine, unflag it as Mine
      Empty n -> Flagged n -- if the tile is Empty, flag it
      Mine -> FlaggedMine -- if the tile is Mine, flag it as FlaggedMine
      _ -> tile -- if the tile is Revealed, do nothing
    newRow = take y (board !! x) ++ [newTile] ++ drop (y + 1) (board !! x) -- build the new row with the new flagged tile
  in
    take x board ++ [newRow] ++ drop (x + 1) board -- build the new board with the new row in correct position


-- reveal a tile method
-- function takes in a Board and a Coordinate and returns a new Board with the tile revealed
revealTile :: Board -> Coordinate -> Board
revealTile board (x, y) =
  let
    tile = board !! x !! y -- get the tile from board
    newTile = case tile of
      Empty n -> Revealed n -- if the tile is empty, reveal it
      _ -> tile -- if the tile is not empty, do nothing
    newRow = take y (board !! x) ++ [newTile] ++ drop (y + 1) (board !! x) -- build the new row with the new revealed tile
    -- dx, dy = -1, 0, 1, guarding that the neighbor coordinates are not 0,0 which is currently investigated tile
    -- also guarding that the neighbor coordinates are within the board boundaries
    -- output of this row is a list of coordinates of neighboring tiles, for example (2,2) -> [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]
    neighbours = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0, x + dx >= 0, x + dx < length board, y + dy >= 0, y + dy < length (head board)]
    newBoard = if tile == Empty 0 then
      foldl revealTile (take x board ++ [newRow] ++ drop (x + 1) board) neighbours -- if the tile is empty and has no mines around it, reveal all neighboring tiles recursively
    else
      take x board ++ [newRow] ++ drop (x + 1) board -- else build the new board with the new row in correct position
  in
    newBoard

-- pad left, adds the pad character padAmount times to the left of the list of characters
padLeft :: Int -> a -> [a] -> [a]
padLeft padAmount padCharacter listOfCharacters =
  replicate (padAmount - length listOfCharacters) padCharacter ++ listOfCharacters

-- print a board and a coordinate system on left and top sides. top = x and left = y
-- function takes in a GameState and a Board and prints the board with the coordinate system
-- mines are only revealed if the GameState is Lost
printBoard :: GameState -> Board -> IO ()
printBoard state board = do
  let width = length $ head board -- width is the length of the first row on Board
  let height = length board -- height is the length of the Board

  let maxCoordinateLength = length $ show $ max height width -- maximum coordinate length (10 = 2 chars, 9 = 1 char, etc.)
  let paddedCoordinatesTop = concatMap (\i -> " " ++ padLeft maxCoordinateLength ' ' (show i) ++ " ") [0..width - 1] -- pad coordinates at the top
  putStrLn $ "    " ++ paddedCoordinatesTop -- print the top coordinates on first row
  putStrLn $ "    " ++ replicate (length paddedCoordinatesTop - 1) '-' -- print a line below the top coordinates, each number takes about 3 dashes

  -- print the board with left coordinate (padded accordingly) and the tiles (padded accordingly to match with top coordinates)
  -- showTile function prints the tile string representation according to game state and tile type
  mapM_ (\(i, row) -> putStrLn $ padLeft maxCoordinateLength ' ' (show i) ++ " |" ++ concatMap (\tile -> " " ++ padLeft maxCoordinateLength ' ' (showTile state tile) ++ " ") row) $ zip [0..] board
  where
    showTile Lost Mine = "x" -- if the GameState is Lost, reveal all Mines
    showTile Lost FlaggedMine = "X" -- if the GameState is Lost, reveal all FlaggedMines
    showTile _ Mine = "*" -- if the GameState is not Lost, show all mines as unrevealed
    showTile _ (Empty _) = "*" -- show all Empty tiles as unrevealed
    showTile _ (Flagged _) = "F" -- show all Flagged tiles as F
    showTile _ FlaggedMine = "F" -- show all FlaggedMines as F
    showTile _ (Revealed 0) = "#" -- show all Revealed tiles with 0 mines around them as #
    showTile _ (Revealed n) = show n -- show all Revealed tiles with mines around them as the number of mines around them


isNumber :: String -> Bool
isNumber s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

-- run the game
main :: IO ()
main = do
  let width = 10 -- width of the board
  let height = 10 -- height of the board
  let mines = 10 -- number of mines to place on the board

  -- if the mine count is more than board size, dont start a game
  if width * height <= mines then do
    putStrLn "Too many mines for the board." -- if the number of mines is greater than the number of tiles on the board, print "Invalid input."
  else do
    randomGenerator <- newStdGen -- initialize new random number generator
    let board = calculateNumbers $ generateBoard width height mines randomGenerator -- generate a board with mines and then calculate the numbers of mines around each tile
    putStrLn "Welcome to Minesweeper!"
    putStrLn "Legend: * - unrevealed, # - revealed, F - flagged, x - mine, number - number of mines around the tile"
    putStrLn "Flagging a tile again unflags it. Flagging all mines wins the game."
    putStrLn ("Number of mines: " ++ show mines)
    putStrLn "Good luck!"
    putStrLn ""
    gameLoop mines board -- start the game loop
    where
      -- game loop
      -- function takes in an integer (mine count) and a Board and returns text
      -- asks the user to enter a coordinate and an action, then reveals or flags the tile
      gameLoop :: Int -> Board -> IO ()
      gameLoop mines board = do
        printBoard Playing board
        putStrLn ""
        putStrLn "Enter a coordinate and action."
        putStrLn "(R)eveal OR (F)lag, e.g. R 2 5 OR F 2 5"
        input <- getLine

        -- parse input
        -- input is space separated, if the input is not 3 words then ask new input
        -- and if the input is too big for the board then ask new input
        let inputWords = words input
        if length inputWords /= 3 then do
          putStrLn "Invalid input."
          gameLoop mines board
        else do
          let action = head inputWords
          let xCheck = inputWords !! 2
          let yCheck = inputWords !! 1

          if isNumber xCheck == False || isNumber yCheck == False then do
            putStrLn "Invalid input."
            gameLoop mines board
          else do
            let x = read (inputWords !! 2) :: Int
            let y = read (inputWords !! 1) :: Int

            -- check if the input is within board bounds
            if x < 0 || x >= length board || y < 0 || y >= length (head board) then do
              putStrLn "Invalid input."
              gameLoop mines board
            else do
              let tile = board !! x !! y
              case action of
                "R" -> do -- action is Reveal
                  if tile == Mine || tile == FlaggedMine then do
                    putStrLn "You lost!" -- if the tile is a Mine or a FlaggedMine, the player loses
                    printBoard Lost board
                  else do
                    let newBoard = revealTile board (x, y) -- else reveal the tile and continue the game
                    gameLoop mines newBoard
                "F" -> do -- action is Flag
                  let newBoard = flagTile board (x, y) -- flag the tile
                  let flaggedMines = length $ filter (== FlaggedMine) $ concat newBoard -- count FlaggedMines in board
                  if flaggedMines == mines then do
                    putStrLn "You won!" -- player wins if there is 10 flagged mines
                    printBoard Won newBoard
                  else do
                    gameLoop mines newBoard -- else continue the game
                _ -> do
                  putStrLn "Invalid input." -- if action is not R or F then print "Invalid input."
                  gameLoop mines board

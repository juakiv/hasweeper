module Functions.BoardGenerator where

import System.Random
import Data.List

import Types
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
module Functions.BoardFunctions where

import Types

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
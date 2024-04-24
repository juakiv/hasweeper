module Functions.BoardPrinter where

import Types
import Functions.Helpers

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

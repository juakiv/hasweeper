module Types where

-- game types, GameState can be Playing, Won or Lost
-- Tile can be Mine, FlaggedMine, Empty with the number of mines around it, Flagged with the number of mines around it,
-- or Revealed with the number of mines around it
-- Board is an array of arrays of Tiles (2D array)
-- Coordinate is a tuple of two integers (x, y)
data GameState = Playing | Won | Lost deriving (Show, Eq)
data Tile = Mine | FlaggedMine | Empty Int | Flagged Int | Revealed Int deriving (Show, Eq)

type Board = [[Tile]]
type Coordinate = (Int, Int)
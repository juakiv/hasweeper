import System.Random
import Data.List

import Types
import Functions.BoardGenerator
import Functions.BoardPrinter
import Functions.BoardFunctions
import Functions.Helpers

-- run the game
main :: IO ()
main = do
  -- ask for width, height and mine count
  putStrLn "Enter width, height and number of mines, e.g. 10 10 10"
  input <- getLine

  -- parse input
  -- input is space separated, if the input is not 3 words then ask new input
  let inputWords = words input
  if length inputWords /= 3 then do
    putStrLn "Invalid input."
    main
  else do
    let widthCheck = inputWords !! 0
    let heightCheck = inputWords !! 1
    let minesCheck = inputWords !! 2

    if isNumber widthCheck == False || isNumber heightCheck == False || isNumber minesCheck == False then do
      putStrLn "Invalid input."
      main
    else do
      let width = read (inputWords !! 0) :: Int
      let height = read (inputWords !! 1) :: Int
      let mines = read (inputWords !! 2) :: Int

      -- if the mine count is more than board size, dont start a game
      if width * height <= mines then do
        putStrLn "Too many mines for the board."
        main
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
                      let totalFlags = countFlaggedTiles newBoard
                      if totalFlags >= mines + 3 then do
                        putStrLn "Too many flags. Flag again to unflag."
                        gameLoop mines board
                      else do
                        let flaggedMines = length $ filter (== FlaggedMine) $ concat newBoard -- count FlaggedMines in board
                        if flaggedMines == mines then do
                          putStrLn "You won!" -- player wins if all mines are flagged
                          printBoard Won newBoard
                        else do
                          gameLoop mines newBoard -- else continue the game
                    _ -> do
                      putStrLn "Invalid input." -- if action is not R or F then print "Invalid input."
                      gameLoop mines board

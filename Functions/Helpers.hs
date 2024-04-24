module Functions.Helpers where

import Types

-- check if a string is a number
isNumber :: String -> Bool
isNumber s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

-- adds the padCharacter padAmount times to the left of the list of characters
padLeft :: Int -> a -> [a] -> [a]
padLeft padAmount padCharacter listOfCharacters =
  replicate (padAmount - length listOfCharacters) padCharacter ++ listOfCharacters

countFlaggedTiles :: Board -> Int
countFlaggedTiles board =
  let
    -- count the number of Flagged with any number and FlaggedMine tiles on the board
    count = length $ filter (\tile -> case tile of Flagged _ -> True; FlaggedMine -> True; _ -> False) $ concat board
  in
    count
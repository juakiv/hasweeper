module Functions.Helpers where

-- check if a string is a number
isNumber :: String -> Bool
isNumber s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

-- adds the padCharacter padAmount times to the left of the list of characters
padLeft :: Int -> a -> [a] -> [a]
padLeft padAmount padCharacter listOfCharacters =
  replicate (padAmount - length listOfCharacters) padCharacter ++ listOfCharacters
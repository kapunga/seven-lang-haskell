module Main where
    revList :: [a] -> [a]
    revList [] = []
    revList (x:y) = revList y ++ [x]

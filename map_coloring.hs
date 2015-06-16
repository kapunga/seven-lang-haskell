module Main where
    -- let diffs  = [("Mississippi", "Tennessee"), ("Mississippi", "Alabama"), ("Alabama", "Tennessee"), ("Alabama", "Georgia"), ("Alabama", "Florida"), ("Georgia", "Florida"), ("Georgia", "Tennessee")]
    flatExcl :: [(String, String)] -> [String]
    flatExcl x = fst (unzip x) ++ snd (unzip x)
    
    setifyImpl :: [String] -> [String] -> [String]
    setifyImpl [] y     = y
    setifyImpl (x:xs) y = if (x `elem` y) then setifyImpl xs y else setifyImpl xs (y ++ [x]) 
    
    setify :: [String] -> [String]
    setify x = setifyImpl x []

    exclusions :: String -> [(String, String)] -> [String]
    exclusions x l = filter (\p -> p /= x) (setify (flatExcl [y | y <- l, (fst y) == x || (snd y == x)]))

    -- The bug is in here!
    filterExcl :: [String] -> [(String, Int)] -> [(String, Int)]
    filterExcl x y =
        let bad = filter (\p -> (fst p) `elem` x) y
        in filter (\q -> not ((snd q) `elem` (snd (unzip bad)))) y

    getColor :: String -> [String] -> [(String, Int)] -> (String, Int)
    getColor x [] _ = (x, 1)
    getColor x _ [] = (x, 1)
    getColor x y z  = 
        let fe  = filterExcl y z
            cmax = (maximum (snd (unzip z))) + 1
            cmin = minimum (snd (unzip fe))
        in if (fe == []) then (x, cmax) else (x, cmin) 

    colorMapImpl :: [(String, Int)] -> [String] -> [(String, String)] -> [(String, Int)]
    colorMapImpl x [] _      = x
    colorMapImpl x (y:ys) z  = colorMapImpl (x ++ [getColor y (exclusions y z) x]) ys z

    colorMap :: [(String, String)] -> [(String, Int)]
    colorMap x = colorMapImpl [] (setify (flatExcl x)) x

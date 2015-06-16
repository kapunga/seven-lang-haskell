module Main where
    posPairs :: [a] -> [(a, a)]
    posPairs []    = []
    posPairs [x]   = []
    posPairs (h:t) = zip (cycle [h]) t ++ posPairs t

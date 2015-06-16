module Main where
    allEvenRecurse :: [Integer] -> [Integer]
    allEvenRecurse []     = []
    allEvenRecurse (x:xs) = if (even x) then x:allEvenRecurse xs else allEvenRecurse xs

    allEvenCompr :: [Integer] -> [Integer]
    allEvenCompr x = [y | y <- x, even y]

    allEvenFilter :: [Integer] -> [Integer]
    allEvenFilter x = filter even x

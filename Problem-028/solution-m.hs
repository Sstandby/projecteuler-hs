
sumDiagonalNumbers :: Integer -> Integer
sumDiagonalNumbers 1 = 1
sumDiagonalNumbers n = go 3 1
    where
        go index acc
            | index > n = acc
            | otherwise = go (index + 2) $! (acc + 4 * index ^ 2 - 6 * index + 6)

result :: Integer
result = sumDiagonalNumbers 1001

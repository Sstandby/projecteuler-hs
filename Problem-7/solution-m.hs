

-- Función que nos dice si un número es primo
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = go ((round.sqrt.fromIntegral) n) 2
    where
        go :: Integer -> Integer -> Bool
        go m d
            | d > (round.sqrt.fromIntegral) n = True
            | mod n d == 0 = False
            | odd d = go m (d + 2)
            | otherwise = go m (d + 1)

-- La lista de los primeros números primos
listOfPrimes :: [Integer]
listOfPrimes = [x | x <- 2 : [3, 5 ..], isPrime x]

-- Respuesta
result :: Integer
result = last (take 10001 listOfPrimes)

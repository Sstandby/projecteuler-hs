
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

-- Suma de numeros primos menores a un número
sumPrimesBellowLimit :: Integer -> Integer
sumPrimesBellowLimit n
    | n == 3 = 5
    | even n = sumPrimesBellowLimit (n - 1)
    | isPrime n = sumPrimesBellowLimit (n - 2) + n
    | otherwise = sumPrimesBellowLimit (n - 2)

-- Respuesta
result :: Integer
result = sumPrimesBellowLimit 2000000


-- Computar la repuesta tarda demasiado (asumo entre 20 y 30 minutos)
-- Se debe mejorar el código

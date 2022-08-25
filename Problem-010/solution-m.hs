
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
sumPrimesBellowLimit n = go n 0
    where
        go n acc
            | n == 3 = acc + 5
            | even n = go (n - 1) acc
            | isPrime n = go (n - 2) $! (acc + n)
            | otherwise = go (n - 2) acc

-- Respuesta
result :: Integer
result = sumPrimesBellowLimit 2000000


-- Computar la repuesta tarda demasiado (asumo entre 20 y 30 minutos)


-- Esta es otra posible respuesta, también tarda mucho

-- La lista de primos que usaremos
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

primes :: [Integer]
primes = sieve [2 ..]

sumPrimesBellowLimit2 :: Integer -> Integer
sumPrimesBellowLimit2 n = sum (takeWhile (<n) primes)

result2 :: Integer
result2 = sumPrimesBellowLimit2 2000000

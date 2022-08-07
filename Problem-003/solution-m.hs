-- Encontraremos al factor primo mayor dividiendo recursivamente al numero dado entre
-- sus factore primos pequeños hasta que no se pueda dividir más

-- La lista de primos que usaremos
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

primes :: [Integer]
primes = sieve [2 ..]

largestPrimeFactor :: Integer -> [Integer] -> Integer
largestPrimeFactor n [] = 0 -- Solo sirve para completar patrones, al final de cuentas la lista de primos es infinita
largestPrimeFactor n (x:xs)
    | mod n x == 0 = largestPrimeFactor (div n x) (x:xs)
    | head xs > n = x
    | otherwise = largestPrimeFactor n xs

-- Respuesta
result :: Integer
result = largestPrimeFactor 600851475143 primes

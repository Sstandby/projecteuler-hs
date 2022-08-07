-- La lista de los primeros números primos
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

primes :: [Integer]
primes = sieve [2 ..]

-- Respuesta
result :: Integer
result = last (take 10001 primes)

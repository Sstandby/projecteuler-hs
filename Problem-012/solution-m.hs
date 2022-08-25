-- Lista infinita de números triangulares
triangularNumbers :: [Integer]
triangularNumbers = scanl1 (+) [1 ..]

-- Lista infinita de números primos
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

primes :: [Integer]
primes = sieve [2 ..]

-- Mayor exponente de un factor primo de un número
maxExponent :: (Integral t, Num p) => t -> t -> p
maxExponent b n
    | mod n b == 0 = 1 + maxExponent b (div n b)
    | otherwise    = 0

-- Factorizarion de un número ([base, exponente])
factorization :: (Integral b, Integral t) => t -> [t] -> [(t, b)]
factorization _ [] = []
factorization 1 xs = []
factorization n (x:xs)
    | mod n x == 0 = (x, exponent) : factorization (div n (x^exponent)) xs
    | otherwise = factorization n xs
        where
            exponent = maxExponent x n

-- Número de divisores de un número
numFactors :: Integer -> Integer
numFactors n = product [e + 1 | (b, e) <- factorization n primes]

-- Función que encuentra el primer número que contenga cierta cantidad de factores
firstNumWithAmountOfFactors :: Integer -> Integer
firstNumWithAmountOfFactors n = head [x | x <- triangularNumbers, numFactors x > n]

-- Respuesta
result :: Integer
result = firstNumWithAmountOfFactors 500

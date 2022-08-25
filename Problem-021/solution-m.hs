
-- Función que quita un elemento de una lista (primera ocurrencia)
removeOne :: Integer -> [Integer] -> [Integer]
removeOne _ [] = []
removeOne x (y:ys)
    | x == y = ys
    | otherwise = y : removeOne x ys

-- Lista infinita de números primos
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

primes :: [Integer]
primes = sieve [2 ..]

-- Factorizarion de un número [(base, exponente)]
maxExponent :: (Integral t, Num p) => t -> t -> p
maxExponent b n
    | mod n b == 0 = 1 + maxExponent b (div n b)
    | otherwise    = 0

factorization :: (Integral b, Integral t) => t -> [t] -> [(t, b)]
factorization _ [] = []
factorization 1 xs = []
factorization n (x:xs)
    | mod n x == 0 = (x, exponent) : factorization (div n (x^exponent)) xs
    | otherwise = factorization n xs
        where
            exponent = maxExponent x n

-- Suma de los divisores propios (Divisores de un número sin incluir al mismo número)
sumProperDivisors :: Integer -> Integer
sumProperDivisors n = sum $ init $ go (factorization n primes) [1]
    where
        go [] result = result
        go (t:ts) result = go ts [m * n | m <- [fst t ^ e | e <- [0 .. (snd t)]], n <- result]

-- Suma de todos los números amigos menores a un límite
sumAmicableNumbers :: Integer -> Integer
sumAmicableNumbers n = go [2 .. (n - 1)] 0
    where
        go [] acc = acc
        go [x] acc = acc
        go (x:xs) acc
            | x == sumProperDivisors s && x /= s = go (removeOne s xs) $! (acc + x + s)
            | otherwise = go xs acc
            where
                s = sumProperDivisors x

-- Respuesta
result :: Integer
result = sumAmicableNumbers 10000




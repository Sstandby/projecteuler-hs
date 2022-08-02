-- Encontraremos al factor primo mayor dividiendo recursivamente al numero dado entre
-- sus factore primos pequeños hasta que no se pueda dividir más

-- Raiz cuadrada redondeada, función para hacer más limpio el código
sqrtRounded :: Integer -> Integer
sqrtRounded n = round (sqrt (fromIntegral n))

-- Creamos la función prime que nos dirá si un número es primo o no
-- (Intente hacer que la función sea lo más rápida posible)
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = go (sqrtRounded n) 2
    where
        go :: Integer -> Integer -> Bool
        go m d
            | d > sqrtRounded n = True
            | mod n d == 0 = False
            | odd d = go m (d + 2)
            | otherwise = go m (d + 1)

-- La lista de primos que usaremos
listOfPrimes :: [Integer]
listOfPrimes = [x | x <- [1 ..], isPrime x]

largestPrimeFactor :: Integer -> [Integer] -> Integer
largestPrimeFactor n [] = 0 -- Solo sirve para completar patrones, al final de cuentas la lista de primos es infinita
largestPrimeFactor n (x:xs)
    | mod n x == 0 = largestPrimeFactor (div n x) (x:xs)
    | head xs > n = x
    | otherwise = largestPrimeFactor n xs

-- Respuesta
result :: Integer
result = largestPrimeFactor 600851475143 listOfPrimes

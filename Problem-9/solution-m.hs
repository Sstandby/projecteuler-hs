
-- Función que calcula el producto de los dígitos de un número
digitsProduct :: Integer -> Integer
digitsProduct n
    | n < 10 = n
    | otherwise = mod n 10 * digitsProduct (div n 10)

-- Listamos todas las ternas que cumplan a < b < c < 1000 y que cumplan el teorema de Pitágoras
listPythagoreanTriplet :: (Num c, Eq c, Enum c) => c -> [(c, c, c)]
listPythagoreanTriplet n = [(a, b, c) | a <- [1 .. (n - 3)], b <- [a .. (n - 2)], c <- [n - a - b], a^2 + b^2 == c^2]

-- Respuesta
result :: Integer
result = a * b * c
    where (a, b, c) = head (listPythagoreanTriplet 1000)


-- Suma de los cuadrados de los primeros n naturales
sumOfSquares :: Integer -> Integer
sumOfSquares n = sum [x^2 | x <- [1 .. n]]

-- Cuadrado de la suma de los primeros n naturales
squareOfTheSum :: Integer -> Integer
squareOfTheSum n = sum [1 .. n] ^ 2

-- Respuesta
result :: Integer
result = squareOfTheSum 100 - sumOfSquares 100

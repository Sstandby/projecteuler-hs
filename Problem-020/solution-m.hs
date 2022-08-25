
-- Función que calcula el factorial de un número
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Digitos de un número
digits :: Integer -> [Integer]
digits n
    | n < 10 = [n]
    | otherwise = digits (div n 10) ++ [mod n 10]

-- Función que calcula la suma de cifras del factorial de un número
sumDigitsOfFactorial :: Integer -> Integer
sumDigitsOfFactorial = sum . digits . factorial

-- Resultado
result :: Integer
result = sumDigitsOfFactorial 100

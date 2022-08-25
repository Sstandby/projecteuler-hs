
-- Este problema se puede resolver con fuerza bruta empezando por el número 2,
-- pero el verdadero problema es encontrar hasta donde probaremos números que cumplan la condición.
-- Por un lado tenemos el número a evaluar y por el otro la suma de sus digitos elevados a cierta potencia.
-- El número crece de forma exponencial al aumentar el número de dígitos mientras que la suma de las potencias
-- de los dígitos crece de forma lineal.
-- Esto quiere decir que habrá un punto donde la suma de las potencias de los dígitos no podrá nunca
-- alcanzar al número. El mejor caso para que tenga más chances de alcanzarlo es cuando todos los dígitos son "9"
-- Compararemos 9 con 9^5, 99 con 9^5 + 9^5, 999 con 9^5 + 5^5 + 9^5, etc. Hasta el punto donde la parte izquierda
-- sea mayor a la derecha y ese será el límite de los números que evaluaremos

-- Función que nos dará el número máximo a evaluar según la potencia utilizada
maxNumberTest :: Integer -> Integer
maxNumberTest p = 9 ^ p * maxNumDigits 1
    where
        maxNumDigits acc
            | (9 ^ p) * acc > 10 ^ acc - 1 = maxNumDigits $! (acc + 1)
            | otherwise                    = acc

-- Función que nos da la suma de las potencias de un dígito
sumOfPowers :: Integer -> Integer -> Integer
sumOfPowers n p = go n 0
    where
        go n acc
            | n == 0 = acc
            | otherwise = go (div n 10) $! (acc + mod n 10 ^ p)

-- Función que suma los números que cumplen la condición
sumPossibleCases :: Integer -> Integer
sumPossibleCases p = sum [x | x <- [2 .. (maxNumberTest p)], x == sumOfPowers x p]

-- Resultado (potencia 5)
result :: Integer
result = sumPossibleCases 5

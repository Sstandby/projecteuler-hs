
-- Lista de números de Fibonnaci
fibList :: [Integer]
fibList = go 0 1
    where
        go a b = a : go b (a + b)

-- Función que encuentra el primer elemento de una lista con "n" digitos y devuelve su posición
indexFirstTermWithNDigits :: Integer -> [Integer] -> Integer
indexFirstTermWithNDigits n l = fst . head . dropWhile (\(i, x) -> x <10^(n-1)) $ zip [1 ..] l

-- Resultado, al resultado lo reducimos en 1 porque en el problema empiezan la secuencia con 1 1 en vez de 0 1
result :: Integer
result = indexFirstTermWithNDigits 1000 fibList - 1


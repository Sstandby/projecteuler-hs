-- Lista de números de Fibonnaci
fibList :: [Integer]
fibList = go 0 1
    where
        go a b = a : go b (a + b)

-- Filtramos los elementos pares
fibListEven :: [Integer]
fibListEven = filter even fibList

-- Sumamos los elementos menores a 4 millones de forma recursiva
result :: Integer
result = go 0 fibListEven
    where
        go _ [] = 0
        go n (x:xs)
            | x < 4000000 = go (n + x) xs
            | otherwise = n



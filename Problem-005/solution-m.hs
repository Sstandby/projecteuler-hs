-- El problema practicamente nos pide que calculemos el mcm de una secuencia de números
-- Haskell nos ofrece una función que nos permite calcular el mcm de dos números,
-- usaremos esa funcion de forma recursiva para hallar el mcm de todos los números

smallesMultiple :: [Integer] -> Integer
smallesMultiple [] = error "Lista Vacía"
smallesMultiple [x] = x
smallesMultiple (x:xs) = lcm x (smallesMultiple xs)

-- Respuesta
result :: Integer
result = smallesMultiple [1 .. 20]

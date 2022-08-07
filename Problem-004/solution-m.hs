-- Empezaremos multiplicando los números más altos hasta llegar a 1x1

-- Funcion que nos dice si un número es palindromo
isPalindrome :: Integer -> Bool
isPalindrome n = (read.reverse.show) n == n

-- Lista de números palindromos posibles al multiplicar dos números de 3 cifras
listPalindromes :: [Integer]
listPalindromes = [x * y | x <- [999, 998 .. 100], y <- [999, 998 .. x], isPalindrome (x * y)]

-- Respuesta
result :: Integer
result = maximum listPalindromes


-- Se puede mejorar, se debe buscar una forma de ordenar el orden en que tomamos 2 números
-- para multiplicarlos, con esa orden solo bastaría usar la funcion head

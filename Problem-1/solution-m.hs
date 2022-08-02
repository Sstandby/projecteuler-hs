-- Sumamos los elementos de una lista de números que son múltiplos de 3 o 5 menores 1 1000

result :: Integer
result = sum [x | x <- [1 .. 999], mod x 3 == 0 || mod x 5 == 0]

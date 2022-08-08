-- Sumamos los elementos de una lista de números que son múltiplos de 3 o 5 menores a 1000
import Data.List
solution = sum $ nub $ [3,6..999] ++ [ 5,10..999]

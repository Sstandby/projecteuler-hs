
import Data.List ( nub )

lenSequence :: Integer -> Integer -> Int
lenSequence x y = length . nub $ [b ^ e | b <- [2 .. x], e <- [2 .. y]]


result :: Int
result = lenSequence 100 100

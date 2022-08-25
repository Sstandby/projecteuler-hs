
-- Función que nos dice si un año es bisiesto
isLeapYear :: Integral a => a -> Bool
isLeapYear year = mod year 4 == 0 && (mod year 100 /= 0 || mod year 400 == 0)

-- Función que nos da el número de días que tiene un mes
numberDaysInMonth :: Int -> Int -> Int
numberDaysInMonth month year
    | month `elem` [4, 6, 9, 11] = 30
    | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | isLeapYear year = 29
    | otherwise = 28

-- Lista de número de días por mes desde el 1 1 1990
daysByMonth :: [Int]
daysByMonth = concat [[numberDaysInMonth month year | month <- [1 .. 12] ] | year <- [1900 ..]]

-- Número de días que cayerón domingo el primero de cada mes desde el 1 1 1990
numberDaysWithSundayAtFirstDay :: Int -> Int
numberDaysWithSundayAtFirstDay lastYear = length (filter (\x -> mod x 7 == 0) (take (12 * (lastYear - 1900 + 1)) (scanl (+) 1 daysByMonth)))

-- Resultado, la primera función toma el rango 1900-2000 y la segunda el rango 1900-1900, el problema pide a partir del
-- año 1901 por eso restamos los casos del año 1900
result :: Int
result = numberDaysWithSundayAtFirstDay 2000 - numberDaysWithSundayAtFirstDay 1900




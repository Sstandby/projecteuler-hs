
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Char ( ord )
import Data.List ( sort )

main :: IO ()
main = do
    handle <- openFile "names.txt" ReadMode -- Problem-022/namex.txt
    contents <- hGetContents handle

    let wordsList = sort (getWords contents)
    print (totalValue wordsList)

    hClose handle

-- Abecedario en mayusculas
asciiUppercase :: String
asciiUppercase = ['A' .. 'Z']

-- Función que obtiene las palabras de un texto
getWords :: String -> [String]
getWords "" = []
getWords text = go text "" []
    where
        go "" "" result = result
        go "" word result = result ++ [word]
        go (x:xs) word result
            | x `elem` asciiUppercase = go xs (word ++ [x]) result
            | word == "" = go xs "" result
            | otherwise = go xs "" $! (result ++ [word])

-- Valor númerico de cada letra: A=1, B=2, etc
charValue :: Char -> Int
charValue c = ord c - 64

alphabeticalValue :: String -> Integer
alphabeticalValue "" = 0
alphabeticalValue (x:xs) = fromIntegral (charValue x) + alphabeticalValue xs

totalValue :: [String] -> Integer
totalValue xs = go xs 1 0
    where
        go [] _ acc = acc
        go (x:xs) index acc = go xs (index + 1) $! (acc + index * alphabeticalValue x)


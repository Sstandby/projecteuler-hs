isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

largest3DigitPalindrome :: Integer
largest3DigitPalindrome = maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

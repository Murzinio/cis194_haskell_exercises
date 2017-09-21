-- validate card number, exercises 1-4

import Data.Digits

toDigits    :: Integer -> [Integer]
toDigits x
    | x <= 0    = []  
    | otherwise = digits 10 x

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x  <= 0   = []
    | otherwise = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther [x] = [x]
doubleEveryOther (x:xs)
    | mod (length xs) 2 == 0 = x     : doubleEveryOther xs
    | mod (length xs) 2 == 1 = x * 2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits     [] = 0
sumDigits    [x] = x
sumDigits (x:xs) = sumDigits(toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x 
    | mod 
        (sumDigits (doubleEveryOther (toDigits x))) 
        10 
        == 0    = True
    | otherwise = False
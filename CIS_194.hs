-- validate card number, exercises 1.1-1.4

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
    

-- print list of optimal moves to solve tower of hanoi, exercise 1.5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- ps - peg source
-- pt - peg target
-- pm - peg middle
hanoi 1 ps pt _ = [(ps, pt)]
hanoi n ps pt pm = hanoi (n-1) ps pm pt  ++ [(ps, pt)] ++ hanoi (n-1) pm pt ps

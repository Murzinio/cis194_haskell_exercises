module Arithmetic where

-- Problem 31
-- Determine whether a given integer number is prime.

isPrimeImp :: Int -> Int -> Bool
isPrimeImp _ 1                = True
isPrimeImp x y
    | even x                  = False
    | x /= y && mod x y == 0  = False
    | otherwise               = isPrimeImp x $ y - 2

isPrime :: Int -> Bool
isPrime x 
    | even half = isPrimeImp x $ half + 1
    | otherwise = isPrimeImp x $ half
        where half = div x 2

-- Testing the above

genPrimesImp :: Int -> [Int] -> [Int]
genPrimesImp 1 ns = ns
genPrimesImp n ns
    | isPrime n = genPrimesImp (n - 1) ns ++ [n]
    | otherwise = genPrimesImp (n - 1) ns

genPrimes :: Int -> [Int]
genPrimes n = genPrimesImp n []

-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

greatestCommonDivisor :: Int -> Int -> Int
greatestCommonDivisor a b
    | a > b = greatestCommonDivisor (a - b) b
    | a < b = greatestCommonDivisor (b - a) a
    | otherwise = a

-- Problem 33
-- Determine whether two positive integer numbers are coprime. 
-- Two numbers are coprime if their greatest common divisor equals 1.

coprime :: Int -> Int -> Bool
coprime a b = greatestCommonDivisor a b == 1

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

totientPhiImp :: Int -> Int -> Int -> Int
totientPhiImp _ 0 c = c  
totientPhiImp m n c
    | coprime m n = totientPhiImp m (n - 1) c + 1
    | otherwise = totientPhiImp m (n - 1) c

totientPhi :: Int -> Int
totientPhi 1 = 1
totientPhi m
    | m <= 0 = error "Invalid input"
    | otherwise = totientPhiImp m (m - 1) 0
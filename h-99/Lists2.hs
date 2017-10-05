module Lists2 where
import Lists

-- Problem 11
-- Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

data EncodedElem a = Single a
           | Multiple Int a
    deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [EncodedElem a]
encodeModified []   = []
encodeModified [x] = [Single x]
encodeModified (x:xs) =
    let elemCount = (1 + getLength (takeWhile (==x) xs))
        in
            if (elemCount == 1)
                then (Single x) : encodeModified (dropWhile (==x) xs)
            else
                (Multiple elemCount x) : encodeModified (dropWhile (==x) xs)

-- Problem 12
-- Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. 
-- Construct its uncompressed version.

decodeModified :: [(EncodedElem a)] -> [a]
decodeModified [] = []
decodeModified [(Single x)] = [x]
decodeModified [(Multiple c x)] = (replicate c x)
decodeModified ((Single x):xs) = [x] ++ decodeModified xs
decodeModified ((Multiple c x):xs) = (replicate c x) ++ decodeModified xs

-- Problem 13
-- Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly. 
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, 
-- but only count them. As in problem P11, 
-- simplify the result list by replacing the singleton lists (1 X) by X.

-- Seems the same as problem solved before.



-- Problem 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x, x]
dupli (x:xs) = [x, x] ++ dupli xs



-- Problem 15
-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli x 1 = x
repli [x] n = [x] ++ repli [x] (n - 1)
repli (x:xs) n = [x] ++ repli [x] (n - 1) ++ repli xs n



-- Problem 16
-- Drop every N'th element from a list.

getHeadNTimes :: [a] -> Int -> [a]
getHeadNTimes _ 0   = []
getHeadNTimes [x] 1 = [x]
getHeadNTimes xs 1   = [head xs]
getHeadNTimes (x:xs) n = x : getHeadNTimes xs (n - 1)

getListWithoutNHeads :: [a] -> Int -> [a]
getListWithoutNHeads [] _ = []
getListWithoutNHeads [_] 1 = []
getListWithoutNHeads xs 0 = xs
getListWithoutNHeads (_:xs) n = getListWithoutNHeads xs (n - 1)

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 1  = []
-- dropEvery (x:_:xs) 2 = x : dropEvery xs 2
-- dropEvery (x:y:_:xs) 3 = x : y : dropEvery xs 3
-- simple cases with n = 2 and n = 3 show emerging pattern that can be used for all n > 1
dropEvery (x:xs) n
    | 1 + (getLength xs) < n = x : xs
    | otherwise              = [x] ++ (getHeadNTimes xs (n - 2)) 
                                   ++ dropEvery (getListWithoutNHeads xs (n - 1)) n


-- Problem 17
-- Split a list into two parts; the length of the first part is given.

-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split xs n 
    | n > getLength xs = error "List too short"
    | otherwise        = (getHeadNTimes xs n, getListWithoutNHeads xs n)

-- Problem 18
-- Extract a slice from a list.

-- Given two indices, i and k, 
-- the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). 
-- Start counting the elements with 1.

getListWithoutNTails :: [a] -> Int -> [a]
getListWithoutNTails [] _        = []
getListWithoutNTails [_] 1       = []
getListWithoutNTails xs 0        = xs
-- getListWithoutNTails xs 1        = init xs
-- getListWithoutNTails xs 2        = init (init xs)
getListWithoutNTails (xs) n
    | getLength xs < n = error "List too short"
    | otherwise        = getListWithoutNTails (init xs) (n - 1)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _  0 0 = [] 
slice xs x y = getListWithoutNTails (getListWithoutNHeads xs (x - 1)) (getLength xs - y)
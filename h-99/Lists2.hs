module Lists2 where
import Lists

-- Problem 11
-- Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. 
-- Only elements with duplicates are transferred as (N E) lists.

data Count = Single
           | Multiple Int
    deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [(Count, a)]
encodeModified []   = []
encodeModified [x] = [(Single, x)]
encodeModified (x:xs) = 
    let elemCount = (1 + getLength (takeWhile (==x) xs))
        in 
            if (elemCount == 1) 
                then (Single, x) : encodeModified (dropWhile (==x) xs)
            else                     
                (Multiple elemCount, x) : encodeModified (dropWhile (==x) xs)
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
decodeModified [(Single x)] = [x]
decodeModified [(Multiple c x)] = (replicate c x)
decodeModified ((Single x):xs) = [x] ++ decodeModified xs
decodeModified ((Multiple c x):xs) = (replicate c x) ++ decodeModified xs
-- Problem 1
-- Find the last element of a list

findLast :: [a] -> a
findLast []  = error "Empty list"
findLast [x] = x
findLast (_:xs) = findLast xs

-- Problem 2
-- Find the last but one element of a list

findLastButOne :: [a] -> a
findLastButOne []     = error "Empty list"
findLastButOne [x, _] = x
findLastButOne (_:xs) = findLastButOne xs

-- Problem 3
-- Find the K'th element of a list. 
-- The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt _  0 = error "Index starts from 1"
elementAt xs x = xs!!(x-1)

-- Problem 4
-- Find the number of elements of a list.

getLength :: [a] -> Int
getLength []     = 0
getLength [_]    = 1 
getLength (_:xs) = getLength xs + 1

-- Problem 5
-- Reverse a list.

reverseList :: [a] -> [a]
reverseList []  = []
reverseList [x] = [x]
reverseList (x:xs) = (reverseList xs) ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverseList x

-- Problem 7
-- Flatten a nested list structure
-- Transform a list, possibly holding lists as elements into a `flat' list 
-- by replacing each list with its elements (recursively).

data NestedList a = Elem a 
                  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List [])       = []
flatten (Elem x)        = [x]
flatten (List (x:xs))   = flatten x ++ flatten (List xs)

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced
-- with a single copy of the element. 
-- The order of the elements should not be changed.

sameAsPrevious:: Eq a => a -> [a] -> Bool
sameAsPrevious x (y:_) = x == y

compress :: Eq a => [a] -> [a]
compress []               = []
compress [x]              = [x]
compress (x:xs)
    | sameAsPrevious x xs = compress xs
    | otherwise           = [x] ++ compress xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.

pack :: Eq a => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- Problem 10
-- Run-length encoding of a list. 
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: Eq a => [a] -> [(Int, a)]
encode []   = []
encode [x] = [(1, x)]
encode (x:xs) = (1 + getLength (takeWhile (==x) xs), x) : encode (dropWhile (==x) xs)
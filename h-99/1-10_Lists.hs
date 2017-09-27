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
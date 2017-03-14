-- #1: Find the last element of a list
myLast :: [a] -> a
myLast []     = error "No empty lists!"
myLast [x]    = x
myLast (_:xs) = myLast xs

-- #2: Find the last but one element of a list.
myButLast :: [a] -> a
myButLast []  = error "No empty lists!"
myButLast [x] = error "List not large enough"
myButLast x   = last (init x)

-- #3: Find the K'th element of a list. The first element in a list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt [] _     = error "Index out of bounds"
elementAt (_:xs) k
  | k < 1          = error "Index out of bounds"
  | otherwise      = elementAt xs (k - 1)
  
-- #4 Find the number of elements of a list.
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- #5 Reverse a list
myReverse :: [a] -> [a]
myReverve []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x] -- Very poor for long lists because of cons operator

-- #6 Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = False
isPalindrome xs   = reverse xs == xs
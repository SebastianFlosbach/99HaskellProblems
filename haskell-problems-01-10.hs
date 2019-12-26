-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Problem 1

myLast :: [a] -> a
myLast [] = error "No last element for empty list"
myLast [a] = a
myLast (_:xs) = myLast xs

-- Problem 2

myButLast :: [a] -> a
myButLast [] = error "No but last element for empty list"
myButLast [x] = error "No but last element for list with only one element"
myButLast x = x !! (length x - 2)

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Can't get element in empty list"
elementAt list index 
 | index > length list = error "Index out of bounds"
 | otherwise = list !! (index - 1)

-- Problem 4

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (_:xs) = 1 + myLength xs

-- Problem 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == (last xs) && (isPalindrome (init xs))

-- Problem 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))

-- Problem 8

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
 | x == y = compress ([x] ++ xs)
 | otherwise = [x] ++ compress([y] ++ xs)
 
-- Problem 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = 
 let (first,rest) = span (==x) xs
 in (x:first) : pack rest

-- Problem 10

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = 
 let (first,rest) = span (==x) xs
 in (length first + 1, x) : encode rest

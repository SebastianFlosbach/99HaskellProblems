-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Problem 11

data Encoding a = Single a | Multiple Int a deriving(Show)

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified [] = []
encodeModified (x:xs)
 | first == [] = Single x : encodeModified xs
 | otherwise = Multiple (length first + 1) x : encodeModified rest
 where (first,rest) = span (==x) xs

-- Problem 12

decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs

-- Problem 13

-- Problem 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list 0 = list
dropEvery (x:xs) n
 | n == 1 = xs
 | otherwise = x : dropEvery xs n

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

-- Aufgabe 16  

dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list n = dropEvery' list n 1
 where 
  dropEvery' :: (Eq a) => [a] -> Int -> Int -> [a]
  dropEvery' [] _ _ = []
  dropEvery' (x:xs) n i
   | i == n = dropEvery xs n
   | otherwise = x : dropEvery' xs n (i + 1)

-- Aufgabe 17

split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split list 0 = (list,[])
split list n
 | n >= length list = ([], list)
 | otherwise = (split' list 0 n 1, split' list (n + 1) (length list) 1)
 where
  split' :: [a] -> Int -> Int -> Int -> [a]
  split' [] _ _ _ = []
  split' (x:xs) start end i
   | i < start = split' xs start end (i + 1)
   | i >= start && i <= end = x : split' xs start end (i + 1)
   | otherwise = []
   
-- Aufgabe 18

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice list start end = slice' list start end 1
 where
  slice' [] _ _ _ = []
  slice' (x:xs) start end i
   | i < start = slice' xs start end (i + 1)
   | i >= start && i <= end = x : slice' xs start end (i + 1)
   | otherwise = []
   
-- Aufgabe 19

rotateLeft :: [a] -> Int -> [a]
rotateLeft [] _ = []
rotateLeft list n = attatch (split list (mod n (length list)))
 where
  attatch (first, last) = last ++ first

-- Aufgabe 20

removeAt :: Int -> [a] -> (a, [a])
removeAt n list
 | n > (length list) = error "Index out of bounds"
 |otherwise = (list !! (n - 1), (slice list 1 (n - 1)) ++ (slice list (n + 1) (length list)))


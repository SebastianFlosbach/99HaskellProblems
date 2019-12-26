-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- Aufgabe 21

insertAt :: a -> [a] -> Int -> [a]
insertAt a [] _ = [a]
insertAt a list i = combine (split list (i - 1)) a
 where
  combine (first, last) a = first ++ [a] ++ last

-- Aufgabe 22

range :: Int -> Int -> [Int]
range a b = [a..b]
 
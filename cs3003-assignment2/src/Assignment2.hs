module Assignment2 where

import Data.List

------------------------------------------------
-- findFirst
-- needle: needle type a return bool 
-- list: haystack type a
-- return Found with two branches: 
-- Match w index i, NoMatch wo index
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst needle list = findIndex needle 0 list -- start index at 0
  where
    findIndex _ _ [] = NoMatch
    findIndex needle i (x:xs) | needle x = Match i -- base case if element x of index i match w check
                              | otherwise = findIndex needle (i + 1) xs -- recursio case when it's not, moving to next element

------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = Span Integer Char deriving Eq
instance Show RunLength where
  show (Span length c) = "Length: " ++ show length ++ ": " ++ show c
runLengthEncode :: [Char] -> [RunLength]
runLengthEncode = map (\str -> Span (genericLength str) (head str)) . group
------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome w = w == reverse w

------------------------------------------------
-- mergesort
-- comp: comparator type a return bool
-- x: list need to sort 
-- return list that is sorted
----------------------------------------------------------------
mergesort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergesort _ [] = []
mergesort _ [x] = [x]
mergesort ord x = merge ord (mergesort ord l) (mergesort ord r)
  where (l,r) = halve ord x


-- support function: divide list into two lists
halve ::(Ord a) => (a-> a -> Bool) -> [a] -> ([a],[a])
halve ord xs = (take lhx xs, drop lhx xs)
  where lhx = length xs `div` 2 

-- support function: from two list compare and merge into one
merge :: (Ord a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ x [] = x
merge _ [] y = y
merge ord (x:xs) (y:ys) | ord x y = x:(merge ord xs (y:ys))
                        | otherwise = (y:merge ord (x:xs) ys)

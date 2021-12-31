module Chapter3 where

compareWithHundled :: Int -> Ordering
compareWithHundled = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWithFunc' :: (a -> a) -> [a] -> [a] -> [(a, a)]
zipWithFunc' _ _ [] = []
zipWithFunc' _ [] _ = []
zipWithFunc' f (x : xs) (y : ys) = (f x, f y) : zipWithFunc' f xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> a) -> [a] -> [a]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

--my filter'
--filter' :: (a -> Bool) -> [a] -> [a]
--filter' f xs = [x | x <- xs, f x]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _f [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    merge :: [Int] -> [Int] -> [Int]
    merge [] b = b
    merge a [] = a
    merge (a : as) (b : bs)
      | a < b = a : merge as (b : bs)
      | otherwise = b : merge (a : as) bs
    left = take halfIndex xs
    right = drop halfIndex xs
    halfIndex = length xs `div` 2

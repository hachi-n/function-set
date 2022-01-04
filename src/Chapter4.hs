module Chapter4 where

listOfFuns = map (*) [0 ..]

--tail recursive
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x : xs) = foldl' f (f v x) xs

--not tail recursive -- stacked!!!
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum xs

sum'' :: [Int] -> Int
sum'' xs = foldl (+) 0 xs

--map' :: (a -> a) -> [a] -> [a]
--map' f [] = []
--map' f (x : xs) = f x : map' f xs
--mapr f = foldr (\x acc -> f x : acc) []
--mapl f = foldl (\acc x -> acc ++ [f x]) []

oddSquareSum = sum (takeWhile (< 10000) (filter odd [1 ..]))

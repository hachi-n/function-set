module Chapter2 where

tell :: (Show a) => [a] -> String
tell [] = "This list is empty."
tell (x : []) = "This list has one element: " ++ show x
tell (x : y : []) = "This list has two elements: " ++ show x ++ "and" ++ show y
tell (x : y : _) = "This list has many elements: " ++ show x ++ "and" ++ show y

tell' :: (Show a) => [a] -> String
tell' [] = "This list is empty."
tell' all@(x : y) =
  "This list elements: "
    ++ show all
    ++ "first element is "
    ++ show x
    ++ "second element is "
    ++ show y

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "This list is empty."
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> Int -> [Int]
replicate' a b = [b | _ <- [1 .. a]]

replicate'' :: Int -> Int -> [Int]
replicate'' a b
  | a <= 0 = []
  | otherwise = b : replicate'' (a -1) b

take' :: Int -> [Int] -> [Int]
take' n (x : xs)
  | n <= 0 = []
  | otherwise = x : take' (n -1) xs

reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (a : as) (b : bs) = (a, b) : zip' as bs

elem' :: (Ord a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = x `elem'` ys

qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot : list) = qsort [a | a <- list, a < pivot] ++ [pivot] ++ qsort [a | a <- list, a >= pivot]

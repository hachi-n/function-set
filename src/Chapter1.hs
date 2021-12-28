module Chapter1 where

generatePair :: Int -> [(Int, Int)]
generatePair x = [(a, b) | a <- [1 .. x], b <- [1 .. x]]

generatePairFilter :: Int -> Int -> [(Int, Int)]
generatePairFilter x y = [(a, b) | (a, b) <- generatePair x, a /= y, b /= y]

generatePairFilter2 :: Int -> Int -> [Int]
generatePairFilter2 x y = [a | (a, 4) <- generatePair x]

hourToMinute :: Float -> Float
hourToMinute x = x * 60

minuteToSecond :: Float -> Float
minuteToSecond x = x * 60

hourToSecond :: Float -> Float
hourToSecond x = hourToMinute (minuteToSecond x)

factorial :: Int -> Int
factorial x = product ([1 .. x])

factorial' :: Int -> Int
factorial' 0 = 1
factorial' x = x * factorial' (x -1)

primeNumberCheck :: Int -> Bool
primeNumberCheck 0 = False
primeNumberCheck 1 = False
primeNumberCheck x = not (primeNumberCheckRecursive x (x `div` 2))
  where
    primeNumberCheckRecursive :: Int -> Int -> Bool
    primeNumberCheckRecursive x 0 = False
    primeNumberCheckRecursive x 1 = False
    primeNumberCheckRecursive x current
      | isDevided current = True
      | otherwise = primeNumberCheckRecursive x (current -1)
    isDevided n2 = x `mod` n2 == 0

generatePrimeNumberList :: Int -> [Int]
generatePrimeNumberList x = [a| a <- [1 .. x], primeNumberCheck a]
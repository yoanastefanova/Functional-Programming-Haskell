main :: IO()
main = do
 print 4
 print (4, 5)
 print (4, 'c')
 print (z6' 3 [1,2,3,4,3,5])
 print (z6' 13 [1,2,3,4,3,5])
 print (z6' 13 [])
 print ( [[(k,x) | k <- [2..4]] | x <- [1..5], odd x] )
 print (take 10 [2..])
 print ([x | (x, i) <- zip [1,3,5,6,7,8] [0..], odd i])
 print (zip [1,3,5,6,7,8] [0..])

v1 :: (Int, Double)
v1 = (2, 4.5)

f :: (Int, Double) -> Double
f (i, m)
 | i > 10    = 2 * m
 | i < 5     = 3 * m
 | otherwise = m

f2 :: (Int, Double) -> Double
f2 (_, m) = m

f3 :: (Int, Double) -> (Int, Double)
f3 (1, _) = (2, 3.5)
f3 v@(2, _) = v
f3 _ = (4, 5.5)

v2 :: [Int]
v2 = [1,2,3,4,5]

v3 :: [[Int]]
v3 = [[1,2],[3],[4,5,6]]

v4 :: [[[[[Int]]]]]
v4 = []

null' :: [Int] -> Bool
null' [] = True
null' _  = False

head' :: [Int] -> Int
head' (p:_) = p

tail' :: [Int] -> [Int]
tail' (_:ps) = ps

f5 :: [Int] -> [Int]
f5 [] = [1]
f5 [1] = [2,3]
f5 [x,y] = [5]
f5 xs@(_:_:_:_:_:_) = xs
f5 (_:_:_:x:_) = [x]
f5 (_:_:z:t:xs) = z:t:4:5:7:[]
f5 (x:y:xs) = [x,y]
f5 (x:_) = [6]
f5 xs = xs

-- 1:2:[] ++ [5,6] -> [1,2,5,6]
{- Зад. 1. Да се напише функция, която намира
           броя на елементите на списък. -}
count :: [Int] -> Int
count []     = 0
count (_:xs) = 1 + count xs
{- Зад. 2. Да се напише функция, която намира
           сумата на елементите в списък. -}
sum' :: [Int] -> Int
sum' []      = 0
sum' (x:xs)  = x + sum' xs

sum'' :: [Int] -> Int
sum'' xs =
 if null xs then 0 else head xs + sum'' (tail xs)

{- Зад. 3. Да се напише функция, която намира
           дали даден елемент се съдържа
           в списък -}
z3 :: Int -> [Int] -> Bool
z3 _ [] = False
z3 k (x:xs) = k == x || z3 k xs

{- Зад. 4. Да се напише функция, която генерира
           списък с простите числа
           в интервала [a,b]. -}
z4 :: Int -> Int -> [Int]
z4 a b
 | a > b     = []
 | isPrime a = a:(z4 (a + 1) b)
 | otherwise = z4 (a + 1) b
 where
  isPrime :: Int -> Bool
  isPrime n = 2 == length [d | d <- [1..n], mod n d == 0]

{- Зад. 5. Да се напише функция, която премахва
           първият елемент равен на x от даден списък. -}
z5 :: Int -> [Int] -> [Int]
z5 _ []     = []
z5 x (a:as) = if x == a then as else a:(z5 x as)

{- Зад. 6. Да се напише функция, която премахва
           всички елементи, които са равни на x от
           даден списък. -}
z6 :: Int -> [Int] -> [Int]
z6 _ []     = []
z6 x (a:as) = if x == a then (z6 x as) else a:(z6 x as)

z6' :: Int -> [Int] -> [Int]
z6' x as = [a | a <- as, a /= x]
myMin:: Int -> Int -> Int
myMin x y  
 | x > y = y
 | otherwise = x
 
 
countDigits :: Int -> Int
countDigits num  
 | num < 10 = 1
 | otherwise = 1 + countDigits (div num 10)
 
sumDigits :: Int -> Int
sumDigits n 
 | n < 10 = n
 | otherwise = (mod n 10) + sumDigits (div n 10)
 
isAscending :: Integer -> Bool
isAscending n
 | n < 10 = True
 | ( mod (div n 10) 10) < (mod n 10) = isAscending (div n 10)
 | otherwise = False
 
 
isDescending :: Integer -> Bool
isDescending n
 | n < 10 = True 
 | ( mod (div n 10) 10) > (mod n 10) = isDescending (div n 10)
 | otherwise = False
 
 
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = (length [x|x <- [2..n-1], mod n x==0]) == 0
-- n = (length [x|x <- [1..n], mod n x == 0]) == 2

isPerfect :: Integer -> Bool
isPerfect n = n == sumDigits [x|x <- [1..n-1], mod n x == 0]
 where 
  sumDigits :: [Integer] -> Integer
  sumDigits [] = 0
  sumDigits (x:xs) = x + sumDigits xs
 


main :: IO()
main = do
 --print(myMin 12 23)
 --print(Digits 123)
 --print(sumDigits 1234)
 --print (isAscending 121)
 --print (isDescending 321)
 --print (isPrime 5)
 --print (isPerfect 6)
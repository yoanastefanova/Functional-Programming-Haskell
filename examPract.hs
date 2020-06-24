-- да се напише ф-я, която намира броя на елементите на списък
len :: [Int] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- намира сумата на числа в списък
sums :: [Int] -> Int
sums [] = 0
sums (x:xs) = x + sum xs

-- намира дали даден елемент се намира в списък
contain :: Int -> [Int] -> Bool
contain _ [] = False
contain n (x:xs) 
 | n == x = True
 | otherwise = contain n xs

-- генерира списък с простите числа в интервала [a,b]
isPrime :: Int -> Bool
isPrime n = (length [x | x <- [2..div n 2], mod n x == 0]) == 0

primesInRange :: Int -> Int -> [Int]
primesInRange a b = [x | x <- [a..b], isPrime x]

-- премахва първия елемент равен на х от даден списък
deleteFirst :: Int -> [Int] -> [Int]
deleteFirst _ [] = []
deleteFirst n (x:xs) 
 | n == x = xs
 | otherwise = x:(deleteFirst n xs)

-- премахва всички елементи, равни на х от даден списък
deleteAll :: Int -> [Int] -> [Int]
deleteAll _ [] = []
deleteAll n (x:xs)
 | n == x = deleteAll n xs
 | otherwise = x:(deleteAll n xs)

--разделя списък на подсписъци с дължина, равна на подаденото число
chunksOf :: Int -> [Int] -> [[Int]]
chunksOf number xs
 | length xs <= number = [xs]
 | otherwise = (take number xs) : (chunksOf number (drop number xs))

-- намира произведението на естествените числа в даден списък, на които
--сумата от делителите е кратна на к

divisors :: Integer -> [Integer]
divisors number = [x | x <- [1..number], mod number x == 0]

prodSumDiv :: [Integer] -> Integer -> Integer 
prodSumDiv xs k = product [x | x <- xs, (sum(divisors x) `mod` k == 0)]

-- интересно число - което се дели без остатък на сумата от цифрите си
-- 410 -> 4+1=5, 410 мод 5 == 0

sumDigits :: Integer -> Integer 
sumDigits number
 | number < 10 = number
 | otherwise = mod number 10 + sumDigits (div number 10)

isInteresting :: Integer -> Bool
isInteresting num = mod num (sumDigits num) == 0


-- връща сумата от целите числа в интервала [a,b], които 
-- са от вида 4к + 1 и в десетичния запис се съдържа 6

--wantedForm :: [Int]
--wantedForm  = [ 4 * k + 1| k <- [1..5]]

{-
sumInInterval :: Integer -> Integer -> Integer
sumInInterval a b 
 | a > b = error "Invalid Input"
 | otherwise = sum [ x | x <- [a..b], mod x 4 == 1, containing x == True]
    where containing num 
           | num < 10 && num /= 6 = False
           | mod num 10 == 6 = True
           | otherwise = containing (div num 10)
-}

-- списък, чиито елементи са непразни списъци от числа и ф-ята връща
-- списък от елементите на първия списък, които представляват 
-- аритметична прогресия

arithmeticProg :: [Integer] -> Bool
arithmeticProg [] = False
arithmeticProg (x:xs) 
 | 





main :: IO ()
main = do

 --print(deleteAll 3 [3, 2, 3, 3, 5, 7, 3])
 --print (chunksOf 3 [1, 2, 3, 4, 5, 6, 7, 8, 9])
 --print (prodSumDiv [1..10] 3)
 --print (isInteresting 25)
 print (sumInInterval )
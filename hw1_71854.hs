import Data.List -- за да използвам genericLength за Integer

-- 1 задача
solveQuadratic :: Double -> Double -> Double -> (Double,Double)
solveQuadratic a b c 
  | discriminant < 0 = error "Discriminant < 0" -- при дискриминанта < 0, корените няма как да се изведат
  | otherwise = (((-b + sqrt discriminant)/(2*a)) , (-b - sqrt discriminant)/(2*a)) where discriminant = (b*b) - (4*a*c)
 
 -- 2 задача, като isPrime ф-ята се използва и надолу
isPrime :: Integer -> Bool
isPrime num = length [x| x <- [2.. div num 2], mod num x == 0] == 0 -- взима числата от 2 до половината на въведеното число, защото делителите на едно число са до неговата среда
 -- и ако не се намерят никакви делители из този интервал, то числото е просто

sumPrimes :: Integer -> Integer -> Integer 
sumPrimes n k 
 | k == 0 = 0 -- ако броят на  числата се изчерпа или първоначално е 0, трябва да се върне нула към сумата 
 | isPrime n == True = n + sumPrimes (n + 1)(k - 1) -- ако въведеното число е просто, то се прибавя и се продължава към следващото, като се намали търсеният брой с едно
 | otherwise = sumPrimes (n + 1) k -- ако въведеното число не е просто, се продължава напред, но без да се намалява броят на търсените
 
-- 3 задача
numDigits :: Integer -> Integer -- ф-я за намиране броя на цифрите в едно число
numDigits num 
 | num < 10 = 1
 | otherwise = 1 + numDigits (div num 10)
 
firstDig :: Integer -> Integer -- ф-я за намиране на първата цифра на число
firstDig num = div num  (10 ^ (numDigits num - 1))
--(div num $ 10 ^ ((numDigits num) - 1)) *  (10 ^ ((numDigits num) - 1)) -- за всички цифри без първата

deleteFirst :: Integer -> Integer -- ф-я, която премахва първата цифра на число
deleteFirst num = num - firstDig num * (10 ^ ((numDigits num) - 1))


isPalindrome :: Integer -> Bool -- ф-я, която проверява дали едно число е палиндром
isPalindrome  num  
 | (num < 10) || ((num > 10 && num < 100) && (mod num 10 == div num 10)) = True -- едноцифрените и двуцифрените числа с еднакви цифри са палиндроми
 | mod num 10 == firstDig num = isPalindrome  $ (deleteFirst num) `div` 10 -- ако последната цифра = първата, се прави рекурсия към проверката за палиндром, като се премахват тези две цифри
 | not (mod num 10 == firstDig num) = False -- ако първата и последната не са равни, то числото не е палиндром


countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b 
 | a > b = error "Invalid input"
 | otherwise = (genericLength [x | x <- [a .. b],  (isPalindrome x == True)]) -- проверяват се всички числа в интервала дали са палиндроми
 -- ако са палиндроми, се запомнят и се взима дължината на списъка, пълен с тези числа

--4 задача
truncatablePrime :: Integer -> Bool
truncatablePrime num 
 | (num < 10 && isPrime num) = True -- ако числото е едноцифрено и просто, то удовлетворява условието
 | isPrime num = truncatablePrime (div num 10) -- ако е просто, се проверява и числото, което е същото, но без последната цифра
 | otherwise = False 
 
 
main :: IO()
main = do
 
 print (solveQuadratic 1 (-5) (-6))
 print (sumPrimes 10 3) 
 print (isPalindrome 123321)
 print (isPalindrome 12321)
 print (isPalindrome 1234561)
 print (isPalindrome 9)
 print (isPalindrome 33)
 print (truncatablePrime 47)
 print (truncatablePrime 3797)
 print (truncatablePrime 317)
 
 
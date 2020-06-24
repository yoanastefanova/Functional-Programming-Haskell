import Data.List

main :: IO()
main = do
 print (isInteresting 410)                                     -- True
 print (isInteresting 15)                                      -- False
 
 print (mySum 5 100)                                           -- 195 = 61 + 63 + 69
 
 print (removeDuplicates [2, 2, 4, 5, 4, 1, 4, 2])             -- [5, 1, 4, 2]
 print (isArith [1, 3, 5, 7])                                  -- True
 print (isArith' [1, 3, 5, 7])                                 -- True
 
 print (arith [[1, 3, 5], [6, 8, 10, 13], [9, 14, 19]])        -- [[1, 3, 5], [9, 14, 19]]
 print (arith' [[1, 3, 5], [(-9), (-7), (-5)], [11, 5, 1, 3]]) -- [[1, 3, 5], [-9, -7, -5]]
 
 print (getUnique [1, 2, 2, 3])                                -- [1, 3]
 print (sumUnique [[1,2,3,2], [-4,-4], [5]])                   -- 1 + 3 + 5 = 9
 
 print (longestSubstring "111224444236555")                    -- 4
 

-- Задача 1.
isInteresting :: Integer -> Bool
isInteresting num = num `mod` (sumDigits num) == 0

sumDigits :: Integer -> Integer
sumDigits num
 | num < 10  = num
 | otherwise = (num `mod` 10) + sumDigits (num `div` 10)

-- Задача 2.
mySum :: Integer -> Integer -> Integer
mySum a b = sum [x | x <- [a..b], (x `mod` 4 == 1), hasSix x]


hasSix :: Integer -> Bool
hasSix num
 | num < 10  = num == 6  -- ако числото е по-малко от 10 остава единствено да проверим дали е 6
 | otherwise = ((num `mod` 10) == 6) || (hasSix (num `div` 10)) 
 
 
-- Задача 3.
arith :: [[Integer]] -> [[Integer]]
arith lst = [x | x <- lst, isArith x]

-- Помощна функция, която проверява дали елементите на списък образуват аритметична прогресия
-- първи вариант
isArith :: [Integer] -> Bool
isArith lst = length (removeDuplicates [y - x | (x, y) <- zip lst (tail lst)]) == 1

-- втори вариант
isArith' :: [Integer] -> Bool
isArith' []         = True                     -- празен списък - образува прогресия
isArith' [_]        = True                     -- списък с един елемент - образува прогресия
isArith' (x1:x2:xs) = helper (x2:xs) (x2 - x1) -- списък с поне два елемента
 where
  helper [_] _ = True                          -- списък с един елемент - образува прогресия
  helper (y2:y3:ys) d = if (y3 - y2) == d      -- списък с поне два елемента
                        then helper (y3:ys) d
                        else False
 
 
-- Помощна функция, която премахва *повтарящите се* елементи от списъка
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates (x:xs)
 | elem x xs = removeDuplicates xs       -- ако x е елемент от опашката => x няма да бъде добавен към резултата, защо?
 | otherwise = x : removeDuplicates xs   -- х не участва в опашката => х се среща само веднъж в списъка, затова го дабявяме към резултата


-- Задача 3. - втори вариант (много готино и хитро решение на Лъчeзар)
arith' :: [[Integer]] -> [[Integer]]
arith' lst = [x | x <- lst, x == [head x, head (tail x) .. (last x)]]

-- Идеята е следната:
-- За всеки подсписък x конструираме списък, чиито първи елемент е първия елемент на х.
-- Вторият елемент е вторият елемент на х - него го получаваме чрез head (tail x)
-- Последния елемент на новият списък искаме да е последният елемент на х - него получаваме чрез last х
-- Защо това работи?
-- Когато конструираме списък по-този начин, т.е когато задаваме стъпката между първия и втория елемент,
-- ще получим списък, чиито елементи образуват наистина аритметична прогресия.
-- Остава само да проверим дали новоконструирания списък е равен на x
-- При равенство, това означава че х е бил аритметична прогресия, и затова го запазваме в резултатния списък



-- Задача 8.
sumUnique :: [[Int]] -> Int
sumUnique lst = sum (concat [getUnique x | x <- lst])

getUnique :: [Int] -> [Int]
getUnique [] = []
getUnique (x:xs)
 | elem x xs = getUnique [y | y <- xs, x /= y]
 | otherwise = x : getUnique xs



-- Задача. Да се намери дължината на най-дългия substring в даден string, състоящ се от еднакви символи.
longestSubstring :: String -> Int
longestSubstring str = maximum [length substring | substring <- (group str)]

main :: IO()
main = do
 print (elemCount [1..10])
 print (chunksOf 3 ['a'..'z']) -- сравненете типа на аргументите с тези на долния ред
 print (chunksOf 4 [1..18])
 print (prodSumDiv [1..10] 3)
 print v
 print p
 print (sumVectors v p)
 print (scaleVector p 3)
 print (dotProduct v p)
 print (crossProduct v p)
 print (magnitude p)
 print (magnitude (0, 0, 1))
 
-- Примери за функции за работа със списъци
isAscending :: Integer -> Bool
isAscending num = helper (zip (digits num) (tail (digits num)))
 where -- вложена дефиниция
  digits x = if x < 10 then [x] else digits (x `div` 10) ++ [x `mod` 10]
  
  helper []                   = True
  helper ((first, second):xs) = (first <= second) && (helper xs) -- обърнете внимание на вложения образец
  
elemCount :: [a] -> Int
elemCount lst = if null lst then 0 else 1 + elemCount (tail lst)

-- Задача 1.
chunksOf :: Int -> [a] -> [[a]]
chunksOf size lst
 | length lst <= size = [lst]
 | otherwise          = (take size lst) : (chunksOf size (drop size lst))
 
 
-- Задача 2.
prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv numbers k = product [number | number <- numbers,
 sum [divisor | divisor <- [1..number], number `mod` divisor == 0] `mod` k == 0]
 -- може да си помислите за по-четимо решение
 
 
-- Задача 3.
type Vector = (Double, Double, Double)

v, p:: Vector
v = (1, 1, 1)
p = (2, 3, 5)

-- a)
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- b)
scaleVector :: Vector -> Double -> Vector
scaleVector (x, y, z) p = (p * x, p * y, p * z)

-- c)
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- d)
crossProduct :: Vector -> Vector -> Vector
crossProduct (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2,
                                          z1 * x2 - x1 * z2,
                                          x1 * y2 - y1 * x2)

-- e)
magnitude :: Vector -> Double
magnitude (x, y, z) = sqrt (x * x + y * y + z * z)
main :: IO()
main = do
 -- Задача 2.
 print (isImage [1, 2, 3] [3, 4, 5])                    -- True
 print (isImage [1, 3 .. 12] [7, 9 .. 18])              -- True
 
 -- Задача 5.
 print (isTriangular [[1, 1, 1], [0, 0, 0], [0, 0, 0]]) -- True
 print (isTriangular [[4, 4], [1, 1]])                  -- False
 
 -- Задача 4.
 print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (0.9, 2)) -- (1.0, 1.0)
 print ((closestPoint [(0, 0), (1, 1), (2, 2)]) (1, 2))   -- (2.0, 2.0) или (1.0, 1.0)?
 
 -- композиция на функции
 print (((\x -> x * x ) . (\y -> y + 5)) 2)   -- 49
 print (((\x -> x + 5) . (\ y -> y * y)) 2)   -- 9
 
 -- пример:
 print (hasAssoc [(1, 2), (2, 3), (3, 4)] 3)  -- True
 print (hasAssoc [(1, 2), (2, 3), (3, 4)] 4)  -- False
 print (findAssoc [(1, 2), (2, 3), (3, 4)] 2) -- 3
 print (findAssoc [(1, 2), (2, 3), (3, 4)] 4) -- 4

-- Пример за списък от двойки
hasAssoc :: Eq a => [(a, a)] -> a -> Bool
hasAssoc list elem = any (\ (first, _) -> first == elem) list

findAssoc :: Eq a => [(a, a)] -> a -> a
findAssoc list elem = if null assoc then elem else head assoc
 where
  assoc = [y | (x, y) <- list, x == elem]


-- Задача 2.
isImage :: [Int] -> [Int] -> Bool
isImage (a:as) (b:bs) = all (== (a - b)) [ai - bi | (ai, bi) <- zip as bs]

-- с ограничение на типа
isImage' :: (Num a, Eq a) => [a] -> [a] -> Bool
isImage' (a:as) (b:bs) = all (== (a - b)) [ai - bi | (ai, bi) <- zip as bs]


-- Задача 4.
type Point = (Double, Double)

closestPoint :: [Point] -> (Point -> Point)
closestPoint ps p@(px, py) = foldr1 chooseCloser ps
 where
  chooseCloser p1 p2 = if (distance p1) < (distance p2) then p1 else p2
  distance (x, y) = sqrt ((px - x) ^ 2 + (py - y) ^ 2)


-- Задача 5.
isTriangular :: [[Int]] -> Bool
isTriangular [row]  = True
isTriangular matrix = (all (== 0) (map head (tail matrix))) && (isTriangular (map tail (tail matrix))) 
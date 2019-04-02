isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = (length[x | x <- [2..n-1], mod n x == 0]) == 0


primesInRange :: Int -> Int -> [Int]
primesInRange a b = [x | x <- [a..b], isPrime x] 


squares :: Double -> Double -> Double -> [(Double, Double)]
squares a b s = [(a, a*a) | a <- [a, a+s..b]]


reverseNumber :: Integer -> Integer 
reverseNumber n = helper n 0
 where
  helper rest acc
  | rest < 10 = rest + acc*10
  | otherwise = helper (div rest 10) ((mod rest 10) + acc * 10)

main :: IO ()
main = do 

--print (primesInRange 1 9)
--print (squares 4 8 2)
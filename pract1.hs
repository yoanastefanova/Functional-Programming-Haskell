sumDiv :: Int -> Int
sumDiv num = sum [divisor | divisor <- [1..num], num `mod` divisor == 0]


prodSumDiv :: [Int] -> Int -> Int
prodSumDiv lst k = product [nums | nums <- lst, (sumDiv nums) `mod` k == 0]


main :: IO()
main = do

 print(sumDiv 4)
 print(prodSumDiv [1..10] 3)
 

 
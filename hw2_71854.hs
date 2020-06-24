-- 2 задача
currFunc :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
currFunc f g 0 x = 0
currFunc f g 1 x = f x
currFunc f g n x = if even n then (g ((currFunc f g (n-1)) x)) else (f ((currFunc f g (n-1)) x))

switchsum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchsum f g 0 x = 0
switchsum f g n x = (currFunc f g n x) + (switchsum f g (n-1) x) 
 
 
 -- 1 задача
 
pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] x = 0
pairCompose (f:g:fs) x = compFirst2Funcs + pairCompose fs x
 where compFirst2Funcs = (f . g) x
pairCompose (f:fs) x = (f . (\x -> x)) x


main :: IO()
main = do

 print(switchsum (\x -> x + 1) (\x -> x * 2) 1 $ 2) 
 print(switchsum (\x -> x + 1) (\x -> x * 2) 2 $ 2)
 print((pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1)

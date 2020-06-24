import Data.Char
main :: IO()
main = do
 --print (makeUpper 'C')
 --print (isNum 'a')
 --print (isCapital 's')
 --print (isLowerLetter 'a')
 print (normalize "Attack London tomorrow at ten a.m.")
 
makeUpper :: Char -> Char
makeUpper ch = chr (ord ch + offset)
 where
  offset = ord 'A' - ord 'a'

isLowerLetter :: Char -> Bool
isLowerLetter ch = ch >= 'a' && ch <= 'z'

isCapital :: Char -> Bool
isCapital ch = ch >= 'A' && ch <= 'Z'
 
isNum :: Char -> Bool
isNum num = num >= '0' && num <= '9'

normalize :: String -> String
normalize [] = []
normalize (x:xs)
 | isDigit x = error "Digits are not allowed"
 | isUpper x = x : normalize xs
 | isLower x = toUpper x : normalize xs
 | otherwise = normalize xs
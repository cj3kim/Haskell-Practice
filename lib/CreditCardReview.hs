
toDigits :: Integer -> [Integer]
toDigits n | n  > 0    = toDigits (n `div` 10) ++ [n `mod` 10]
           | otherwise =  []


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther  n = reverse (zipWith (*) (reverse n) (cycle([1,2])))


computeChecksum :: Integer -> Integer
computeChecksum n = sum (doubleEveryOther (toDigits n))

validateChecksum :: Integer -> Bool
validateChecksum n = if n `mod` 10 == 0 then True else False




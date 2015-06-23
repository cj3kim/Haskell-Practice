

computeLastIndex :: [Int] -> Int
computeLastIndex a = (length a)

computeParallelRange :: [Int] -> [Int]
computeParallelRange a = [1..(computeLastIndex a)]

computeIndexedList :: [Int] -> [(Int, Int)]
computeIndexedList a = [(i, x) | (i, x) <- zip (computeParallelRange a) a]


isEven :: Int -> [Int] -> Bool
isEven i a | i `mod` 2 == 0 = True
           | otherwise      = False


doubleMe a = [ if c then x*2 else x | pair <- computeIndexedList a,
                                            let i = (fst pair),
                                            let x = (snd pair),
                                            let c = isEven i a ]
computeCheckSum :: [Int] -> Int
computeCheckSum a = sum (doubleMe a)

isCheckSumValid :: Int -> Bool
isCheckSumValid checksum = if checksum `mod` 10 == 0 then True else False

main = print (isCheckSumValid (computeCheckSum [1,2,3,4]))

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse
                 . zipWith (*) (cycle [1,2])
                 . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum
          . concatMap toDigits

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

module HW4 where

import Data.List

fun1' :: [Integer] -> Integer
fun1' = product . map (-2+) . filter (even)

fun2' :: Integer -> Integer
fun2' n = sum . filter even $ takeWhile (>1) $ iterate nextStep n
          where nextStep n = if even n then n `div` 2 else 3 * n + 1

module HW4 where

import Data.List

fun1' :: [Int] -> Int
fun1' = product . map (-2+) . filter (even)

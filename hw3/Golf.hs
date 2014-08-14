module Golf where
import Data.List (transpose)

-- list skipping

skips :: [a] -> [[a]]
skips xs = map (every xs) [1..(length xs)]

everyf :: [a] -> Int -> [a]
everyf [] _ = []
everyf xs n = head xs : everyf (drop n xs) n
every xs n = everyf (drop (n - 1) xs) n

-- find the local maxima of a list

localMaxima :: [Integer] -> [Integer]
localMaxima z = map (\(a,b,c) -> b)
                . filter (\(x,y,z) -> x < y && y > z)
                $ zip3 z (drop 1 z) (drop 2 z)

-- histogram

occurs :: [Int] -> Int -> Int
occurs xs n = length $ filter (== n) xs

histogram :: [Int] -> String
histogram xs = unlines
             . reverse
             . (:) "==========\n0123456789"
             . transpose
             $ zipWith (++) x y
          where c = map (occurs xs) [0..9]
                x = map (flip replicate '*') c
                y = map (flip replicate ' ') $ map ((-) $ maximum c) c

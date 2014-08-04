module Golf where

skips :: [a] -> [[a]]
skips xs = map (every xs) [1..(length xs)]

everyf :: [a] -> Int -> [a]
everyf [] _ = []
everyf xs n = head xs : everyf (drop n xs) n
every xs n = everyf (drop (n - 1) xs) n

localMaxima :: [Integer] -> [Integer]
localMaxima z = map (\(a,b,c) -> b)
                . filter (\(x,y,z) -> x < y && y > z)
                $ zip3 z (drop 1 z) (drop 2 z)

occurs :: [Int] -> Int -> Int
occurs xs n = length $ filter (== n) xs

hist l = map (occurs l) [0..9]

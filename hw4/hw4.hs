module HW4 where

import Data.List ((\\))

-- Exercise 1: Wholemeal Programming

fun1' :: [Integer] -> Integer
fun1' = product . map (-2+) . filter (even)

fun2' :: Integer -> Integer
fun2' n = sum . filter even $ takeWhile (>1) $ iterate nextStep n
          where nextStep n = if even n then n `div` 2 else 3 * n + 1

-- Exercise 2: Folding with Trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree:: [a] -> Tree a
foldTree = foldr insert Leaf

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 1 Leaf a Leaf
insert a (Node h lt n rt)
  | hl < hr = Node h (insert a lt) n rt
  | hl > hr = Node h lt n newtree
  | otherwise = Node (newheight+1) lt n newtree
  where hl = height lt
        hr = height rt
        newtree = insert a rt
        newheight = height newtree

-- Exercise 3: More Folds!

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' = flip foldr [] . ((:) .)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f s l = foldr (flip f) s (reverse l)

-- Exercise 4: Finding Primes

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) filtered
  where filtered = [1..n] \\ specials
        specials = [ i+j+2*i*j | i <- [1..n], j <- [1..n], isValid i j n ]
        isValid i j n = i <= j && (i+j+2*i*j) <= n

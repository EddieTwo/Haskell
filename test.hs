length' 	:: [a] -> Integer
length' [] 	= 0
length' (x:xs) 	= 1 + length' xs

head' 		:: [a] -> a
head' (x:xs) 	= x

tail' 		:: [a] -> [a]
tail' (x:xs) 	= xs

data Point a 	= Point a a
data Color   	= Red | Green | Blue | Indigo | Violet

-- recursive type
data Tree a 	= Leaf a | Branch (Tree a) (Tree a)

fringe 				:: Tree a -> [a]
fringe (Leaf x)			= [x]
fringe (Branch left right) 	= fringe left ++ fringe right

quicksort 		::(Ord a) => [a] -> [a] 
quicksort []		= []
quicksort (x:xs)	=  quicksort[y | y <- xs, y<x]
			++ quicksort[x]
			++ quicksort[y | y <- xs, y>=x]

add 			:: Integer -> Integer -> Integer
add x y			= x + y

-- (**)			:: [a] -> [a] -> [a]
-- [] ** ys		= ys
-- (x:xs)	** ys		= x: (xs ** ys)




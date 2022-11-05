mergesort :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a]
mergesort cmp [] = []
mergesort cmp [x] = [x]
mergesort cmp xs
  = merge cmp (mergesort cmp ys) (mergesort cmp zs)
    where
    (ys, zs) = (take n xs, drop n xs)
    n = length xs `div` 2

merge :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a] -> [a]
merge cmp [] ys = ys
merge cmp xs [] = xs
merge cmp (x:xs) (y:ys)
    | cmp x y = x : merge cmp xs (y:ys)
    | otherwise = y : merge cmp (x:xs) ys

--q1
my_function :: (String, Int) -> (String, Int) -> Bool
my_function (_, g1) (_, g2) = g1 < g2

--mergesort my_function [("Sam", 46), ("Bob", 22), ("Alice",65), ("George", 87), ("Jason", 77)]
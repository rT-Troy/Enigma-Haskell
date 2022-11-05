import Data.ByteString.Lazy.Char8 (foldr')
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

--q2
map' f xs = foldr ((:).f) [] xs
map2 f = foldr((:).f) []
filter' p ys = foldr (\x xs -> if p x then x : xs else xs) [] ys
filter2 p  = foldr (\x xs -> if p x then x : xs else xs) [] 

--q3
data Shape = Circle Float | Rect Float Float deriving Show
scale :: Float -> Shape -> Shape
scale f (Circle circleF) = Circle (circleF*f)
scale f (Rect rectA rectB) = Rect (rectA*f) (rectB*f)
-- > scale 2.0 (Circle 1)
-- > scale 2.0 (Rect 1 2)

--q4
mapq4 xs = [x+3 | x<-xs]
filterq4 xs = [x | x<-xs, x>7]

concat' xs ys = (map (\x -> map (\y -> (x,y)) ys) xs)   --great example for anonymous function
-- > concat' [4,5,6] [1,2,3]
concatq4 xs ys  = [(x,y) | x<-xs, y<-ys]
funcq4 = concatMap (\x -> [(x,x+2,x/2)]) [1,3,5]

-- >filter (>3) (map (\(x,y) -> x+y) xys)
filterq4' xys = [x+y | (x,y)<-xys, x+y > 3]

--q5
mystery xs = foldr (++) [] (map sing xs) where sing x = [x]
{-
mystery [1,2,3]
 = foldr (++) [] (map sing xs)
 = foldr (++) [] [[1],[2],[3]]
 = [1] ++ (foldr (++) [] [[2],[3]])
 = [1] ++ ([2] ++ (foldr (++) [] [[3]]))
 = [1] ++ ([2] ++ ([3] ++ (foldr (++) [] [])))
 = [1] ++ ([2] ++ ([3] ++ []))
 = [1,2,3] -- the same list!
-}

--q6
maximum' xs = foldl1 max xs

--q7

curry' :: ((a, b) -> t) -> a -> b -> t
curry' f x y = f (x,y)

uncurry' :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry' f (x, y) = f x y

--q8

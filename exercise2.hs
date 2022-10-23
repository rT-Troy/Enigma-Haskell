

module Exercise2 where

  import Data.Char
  
  let2int :: Char -> Int
  let2int c = ord c - (ord 'a')

  int2let :: Int -> Char
  int2let n = chr (ord 'a' + n)

  --q1
  encode :: [Char] -> [Int]
  encode xs = [let2int x | x<- xs]

  --q2
  decode :: [Int] -> [Char]
  decode xs = [int2let x | x<- xs]

  --q3
  fibonacci 0 = 1
  fibonacci 1 = 1
  fibonacci n = fibonacci (n-1) + fibonacci (n-2)


  --q4
  find' n xs = show (xs !! n) 
   --show will converts to a String, so could be better like below

  nth 0 (x:xs) = x
  nth n (x:xs) = nth (n-1) xs

  --q5
  roots :: (Float, Float, Float) -> (Float, Float)
  roots (a,b,c) = (x1,x2)
    where x1 = (-b+sqrt (b^2-4*a*c))/2*a
          x2 = (-b-sqrt (b^2-4*a*c))/2*a
  --could be more clear like below

  rootsB (a, b, c) = ((-b+s)/d,(-b-s)/d)
    where s = sqrt (b*b -4.0*a*c)
          d = 2.0*a


  --q7
  rewrite = map (2*) (filter even [1..20])
   --good

  --q8

  --q9
  --ghci> (\n -> n+1) 5
  --ghci> (\n -> n-1) 5
  --ghci> (\n -> [n%x == 0| x<- [2..n]])   --not work

{-
(Harder!) Check if a value is a prime number
OK… start with a lambda expression to check if a number n is divisible by another, a:
(\ n a -> n `mod` a == 0)
Now generate all the factors of a number p (except 1 and p itself):
(\ p -> filter (\ a -> p `mod` a == 0) [2..p `div` 2])
The number is prime if that list is empty:
(\ x -> null (
 (\ p -> filter (\ a -> p `mod` a == 0) [2..p `div` 2]) x))
Still not quite right! We only want numbers > 1:
(\ x -> x > 1 && null (
 (\ p -> filter (\ a -> p `mod` a == 0) [2..p `div` 2]) x))
-}
  --q10.1  
  luhnDouble :: Int -> Int
  luhnDouble n 
    | 2*n > 9 = 2*n-9
    | otherwise = 2*n

  luhnDouble2 n
    | x > 9 = x - 9
    | otherwise = x
    where x = 2*n
  
  --q10.2 --don't really understand the meaning of part 2

  --q11

  length' :: [a] -> Int
  length' [] = 0
  length' (x:xs) = 1 + length xs

  --that's wasted

  --q12
  {-
  --euclid a b = 

  --maxConve n = filter True haveMaxCon
  pairs boo xs = zip haveMaxCon xs where xs = [0..20]
  haveMaxCon n = [n`mod`xs == 0 | xs <- [(n-1)..1]]
  -}

  --q13
  --p a
  sum' [] = 0
  sum' (x:xs) = x + (sum' xs)
    --good 
  --reminder!!: when do the recursion, it's important to make sure non-exhaustive patterns happended like remember [] case.
  --p b  --s:start --h:how much
  
  take' xs s h = take h (drop s xs)

  take2 :: Int -> [a] -> [a]
  take2 0 _ = []
  take2 n (x:xs) = x : take2 (n-1) xs

  --p c
  
  --last' xs = 

  last' :: [a] -> a
  last' [] = error "Insufficient items in input list"
  last' [x] = x
  last' (x:xs) = last' xs


  --not finished 
  --q14
  zip' :: [a] -> [b] -> [(a,b)]
  zip' [] _ = []
  zip' _ [] = []
  zip' (a:as) (b:bs) = (a,b) : zip' as bs



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

  --q5
  roots :: (Float, Float, Float) -> (Float, Float)
  --roots a b c = show (zip [(-b+sqrt (b^2-4*a*c))/2a] [(-b-sqrt (b^2-4*a*c))/2a])
  roots (a,b,c) = (x1,x2)
    where x1 = (-b+sqrt (b^2-4*a*c))/2*a
          x2 = (-b-sqrt (b^2-4*a*c))/2*a
  --q7
  rewrite = map (2*) (filter even [1..20])

  --q8

  --q9
  --ghci> (\n -> n+1) 5
  --ghci> (\n -> n-1) 5
  --ghci> (\n -> [n%x == 0| x<- [2..n]])   --not work


  --q10.1  
  luhnDouble :: Int -> Int
  luhnDouble n 
    | 2*n > 9 = 2*n-9
    | otherwise = 2*n
  
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
  --reminder!!: when do the recursion, it's important to make sure non-exhaustive patterns happended like remember [] case.
  --p b  --s:start --h:how much
  
  take' xs s h = take h (drop s xs)

  --p c
  
  last' xs = 


  

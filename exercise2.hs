

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
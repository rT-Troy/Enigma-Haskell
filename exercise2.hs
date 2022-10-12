

module Exercise2 where

  import Data.Char
  
  let2int :: Char -> Int
  let2int c = ord c - (ord 'a')

  int2let :: Int -> Char
  int2let n = chr (ord 'a' + n)

  --exercise 2 q1.1
  encode :: [Char] -> [Int]
  encode xs = [let2int x | x<- xs]

--exercise 2 q1.2
  decode :: [Int] -> [Char]
  decode xs = [int2let x | x<- xs]
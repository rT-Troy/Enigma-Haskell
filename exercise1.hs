
module Exercise1 where
  --q1
  second xs = head (tail xs)
  swap (x,y) = (y,x)
  pair x y = (x,y)
  double x = x*2
  palin xs = reverse xs == xs
  twice f x = f (f x)

  --q2
  eucDistance (a,b) (c,d) = sqrt(fromIntegral (c-a)^2+(d-b)^2) --remove \fromIntegral\ is still works


  --q3
  firstWorld xs = takeWhile (/=' ') xs

  --q4
  halve ::  [a] -> ([a],[a])
  halve xs = splitAt (length xs `div`2) (xs)

  --q8

  mean xs = (sum xs) `div` (length xs)

  --q9
  challenge xs =  (snd xs)

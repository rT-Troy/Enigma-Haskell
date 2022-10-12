
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

  --q5
  grade1 xs 
    | (sum xs)/4>= 70  = "H1"
    | (sum xs)/4>= 60 && (sum xs)/4<= 69 = "H2"
    | (sum xs)/4>= 50 && (sum xs)/4<= 59 = "H3"
    | (sum xs)/4>= 45 && (sum xs)/4 <= 49 = "H4"
    | (sum xs)/4>= 40 && (sum xs)/4 <= 44 = "Pass"
    | otherwise = "Fail"

  grade2 [a,b,c,d] = if ((a+b+c+d)/4) >= 70 then "H1" 
    else if ((a+b+c+d)/4)>=60 && ((a+b+c+d)/4)<=69 then "H2"
    else if ((a+b+c+d)/4)>=50 && ((a+b+c+d)/4)<=59 then "H3"
    else if ((a+b+c+d)/4)>=45 && ((a+b+c+d)/4)<=49 then "H4"
    else if ((a+b+c+d)/4)>=40 && ((a+b+c+d)/4)<=44 then "Pass"
    else "Fail"
    -- could also use 'where'


  --q8

  grade3 ::  [Float] -> String
  grade3 marks
    | mean>= 70  = "H1"
    | mean>= 60 && mean<= 69 = "H2"
    | mean>= 50 && mean<= 59 = "H3"
    | mean>= 45 && mean<= 49 = "H4"
    | mean>= 40 && mean<= 44 = "Pass"
    | otherwise = "Fail"
    where mean = (sum marks) / fromIntegral(length marks)



  --q9
  challenge marks
    | mean>= 70  = "H1"
    | mean>= 60 && mean<= 69 = "H2"
    | mean>= 50 && mean<= 59 = "H3"
    | mean>= 45 && mean<= 49 = "H4"
    | mean>= 40 && mean<= 44 = "Pass"
    | otherwise = "Fail"
    where mean = sum (map weighted marks)/ fromIntegral (sum (map snd marks))
    
  weighted mark = fst mark * fromIntegral (snd mark)
  --don't really understand the meaning
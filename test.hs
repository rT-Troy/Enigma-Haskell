


module Test where
  
  -- Q2
  dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2+(y2-y1)^2) 
  
  -- Q3
  


  -- Q4
  --halve :: [a] -> ([a],[a])
  --splitAll
  
  -- Q5
  result [a,b,c,d] = if ((a+b+c+d)/4) >= 70 then "H1" 
    else if ((a+b+c+d)/4)>=60 && ((a+b+c+d)/4)<=69 then "H2"
    else if ((a+b+c+d)/4)>=50 && ((a+b+c+d)/4)<=59 then "H3"
    else if ((a+b+c+d)/4)>=45 && ((a+b+c+d)/4)<=49 then "H4"
    else if ((a+b+c+d)/4)>=40 && ((a+b+c+d)/4)<=44 then "H5"
    else "Fail"

  -- How about if we wanted all numbers from 50 to 100 whose remainder when divided with the number 7 is 3?
  test xs = [s | s <- [50..100], s `mod` 7  ==  3 ] -- `` is different of '' ,mod means '%' in java
  moreCondi xs =[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  -- could ave more condition, so all conditions after |
  -- with if then else
  boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   


  let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
  [ [ x | x <- xs, even x ] | xs <- xxs]  -- Nested list comprehensions
  [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]] 

  -- fst snd
  {- zip  -- can zip finite lists with infinite lists
  ghci>  zip [ 1 , 2 , 3 , 4 , 5 ] [ 5 , 5 , 5 , 5 , 5 ]  
  [( 1 , 5 ),( 2 , 5 ),( 3 , 5 ),( 4 , 5 ),( 5 , 5 )] 
  -}

  --Haskell can solve complex math questions



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


  --let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
  --[ [ x | x <- xs, even x ] | xs <- xxs]  -- Nested list comprehensions
  --[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]] 

  -- fst snd
  {- zip  -- can zip finite lists with infinite lists
  ghci>  zip [ 1 , 2 , 3 , 4 , 5 ] [ 5 , 5 , 5 , 5 , 5 ]  
  [( 1 , 5 ),( 2 , 5 ),( 3 , 5 ),( 4 , 5 ),( 5 , 5 )] 
  -}

  --Haskell can solve complex math questions
  triangle = [(x,y,z) | x<-[1..10],y<-[1..10],z<-[1..10],x^2 == y^2 + z^2,x>y,y>z]

  removeNonUppercase :: [Char] -> [Char]   
  removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

  --let in

  capital1 :: String -> String  
  capital1 " " = "Empty string, whoops!"
  capital1 (x:xs) = "The first letter of " ++ xs ++ " is " ++ [x]  --xs has been removed the first letter
  
  capital2 :: String -> String  
  capital2 " " = "Empty string, whoops!"
  capital2 all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

  maximum' :: (Ord a) => [a] -> a   
  maximum' [] = error "maximum of empty list"   
  maximum' [x] = x   
  maximum' (x:xs)    
    | x > maxTail = x   
    | otherwise = maxTail   
    where maxTail = maximum' xs


  replace :: String -> String -> String -> String
  replace orig new [] = []
  replace orig new (x:xs)
    | orig == prefix = new ++ replace orig new rest
    | otherwise = x : replace orig new xs
    where (prefix,rest) = splitAt (length orig) (x:xs)


  primes' :: [Int]
  primes' = sieve [2..100]
    where sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p > 0 ]
module Bags where
  
  type Bag a = [(Int,a)]

  listToBag :: Eq a => [a] -> Bag a
  listToBag [] = []
  listToBag (x:xs) = bagInsert x (listToBag xs)


  bagInsert :: Eq a => a -> Bag a -> Bag a
  bagInsert x [] = [(1,x)]
  bagInsert x ((n,v):rest)
    | x == v = (n+1,v):rest
    | otherwise = (n,v):(bagInsert x rest)
  
  bagInsertN :: Eq a => a -> Int -> Bag a -> Bag a
  bagInsertN x n [] = [(n,x)]
  bagInsertN x n ((n1,v1):rest)
    | x ==v =[(n+n1,x)]
    |otherwise = (n1,v1):(bagInsertN x n rest) 

  bagSum :: Eq a => Bag a -> Bag a -> Bag a
  bagSum b1 [] = b1
  bagSum [] b2 = b2
  bagSum ((n,v):rest) b2 = bagSum rest (bagInsertN v n b2)

  bagIntersection :: Eq a =>Bag a -> Bag a -> Bag a 
  bagIntersection b1 b2 = [(min n1 n2,v1)| (n1,v1)<-b1, (n2,v2)<-b2, v1==v2]
  
  bagEqual :: Eq a => Bag a -> Bag a -> Bool
  bagEqual b1 b2 = (length b1 == length b2) && and [n1==n2 && v1==v2 | (n1,v1)<-b1,(n2,v2)<-b2]
